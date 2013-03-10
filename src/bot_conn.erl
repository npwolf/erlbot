%%% bot_conn manages the socket connection to the irc server
%%% Connect, Reconnect is handled here.  All messages are sent
%%% to irc_router for parsing.  Outgoing messages are sent
%%% via bot_conn, but through irc_send.
-module(bot_conn).
-behavior(gen_server).
% 30 seconds
-define(RECONNECT_AFTER, 30000).
-export([start_link/1, stop_bot/0, send/1, connect/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                 terminate/2, code_change/3]).
-record(state, {server, port, sock}).

start_link([Server, Port]) -> 
    gen_server:start_link({local, bot_svc}, ?MODULE, [Server, Port], []).

init([Server, Port]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    connect(Server, Port),
    io:format("[~s] Started~n", [?MODULE]),
    {ok, #state{server=Server, port=Port}}.

% Connect to an IRC server with a given Host and Port.  Set up the TCP option to
% give us messages on a line-by-line basis.
connect(Host, Port) ->
    TcpOptions = [binary, {active, true}, {packet, line}, {keepalive, true}], 
   io:format("[~s] Connecting to ~s:~p~n", [?MODULE, Host, Port]),
    case gen_tcp:connect(Host, Port, TcpOptions) of
        {ok, Sock} -> 
            gen_server:cast(bot_svc, {new_sock, Sock}),
            irc_router:connected();
        {error, Reason} ->
           io:format("[~s] Error connecting to ~s:~p Reason: ~p~n", [?MODULE, Host, Port, Reason]),
           reconnect()
    end,
    ok.

reconnect() ->
    io:format("Waiting ~p seconds before reconncting~n", [?RECONNECT_AFTER]),
    erlang:send_after(?RECONNECT_AFTER, bot_svc, {connect}).

%%% DO NOT CALL this, use irc_send instead
send(Line) ->
    gen_server:cast(bot_svc, {raw_send, Line}),
    ok.

handle_cast({new_sock, Sock}, S) ->
    io:format("[~w] New Sock~n", [Sock]),
    {noreply, S#state{sock=Sock}};
handle_cast({raw_send, Msg}, S = #state{sock=Sock}) ->
    io:format("[~w] send: ~p~n", [Sock, Msg]),
    gen_tcp:send(Sock, Msg),
    {noreply, S};
handle_cast(Msg, S = #state{sock=Sock}) ->
    io:format("[~w] Unknown cast: ~p~n", [Sock, Msg]),
    {noreply, S}.

handle_info({tcp, Sock, Data}, S = #state{sock=Sock}) ->
    %%io:format("[~w] Sock Received: ~s", [Sock, Data]),
    irc_router:recv_raw(Data),
    {noreply, S};
handle_info({tcp_closed, Sock}, S = #state{sock=Sock}) ->
    io:format("[~w] Disconnected.~n", [Sock]),
    irc_router:disconnected(),
    flush(),
    reconnect(),
    {noreply, S};
handle_info({connect}, S = #state{server=Server, port=Port}) ->
    flush(),
    connect(Server, Port),
    {noreply, S};
handle_info(Msg, S = #state{sock=Sock}) ->
    io:format("[~w] Unexpected message: ~p~n", [Sock, Msg]),
    {noreply, S}.

handle_call(terminate, _From, S) ->
    {stop, normal, ok, S}.

%% Flush all messages so they dont queue up
flush() ->
    receive
        _ -> 
            io:format("Flushing...~n"),
            flush()
    after 0 ->
            ok
    end.

%% On termination we'll quickly send a random fact as our quit message
terminate(Reason, _S = #state{sock=Sock}) ->
    io:format("[~s] Shutdown Started Reason: ~p~n", [?MODULE, Reason]),
    Fact = erlangfacts:getrandom(),
    Msg = <<"QUIT :", Fact/binary, "\r\n">>,
    gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    io:format("[~s] Shutdown Complete.~n", [?MODULE]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

stop_bot() ->
    gen_server:call(bot_svc, terminate).
