%%% nick_plugin handle setting nick when we connect
%%% handles if nick in use by appending a number to pref nick
-module(nick_plugin).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {pref_nick, current_nick, nick_number=0}).
-include("bot.hrl").

%% gen_server specfic
start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    io:format("[~s] started.~n", [?MODULE]),
    irc_router:add_sub(self()),
    % Load our nick from settings
    Nick = settings:get(?MODULE, nick),
    BinNick = list_to_binary(Nick),
    {ok, #state{pref_nick=BinNick}}.

%% 433    ERR_NICKNAMEINUSE
handle_cast({irc_router, msg_rec, #irc_msg{cmd = <<"433">>}}, S = #state{pref_nick=PNick, current_nick=CNick, nick_number=Num}) ->
    io:format("[~s] Nickname in use ~s~n", [?MODULE, CNick]),
    NickNum = Num + 1,
    NickNumBin = list_to_binary(integer_to_list(NickNum)),
    NewNick = << PNick/binary, NickNumBin/binary >>,
    set_nickname(NewNick),
    NewState = S#state{current_nick=NewNick, nick_number=NickNum},
    {noreply, NewState};
handle_cast({irc_router, connected}, S = #state{pref_nick=Nick}) ->
    io:format("[~s] Changing nick to ~s~n", [?MODULE, Nick]),
    set_nickname(Nick),
    % This only needs to be done once, or else you get 462 already registered
    irc_send:raw_unbuf(<< "USER ", Nick/binary, " hostname servername :", Nick/binary >>),
    {noreply, S#state{current_nick=Nick, nick_number=1}};
handle_cast({irc_router, disconnected}, S) ->
    {noreply, S#state{current_nick = <<>>}};
handle_cast(_Msg, S = #state{}) -> 
    {noreply, S}.

handle_info(Msg, S) ->
    io:format("[~s] Unknown info ~p~n", [?MODULE, Msg]),
    {noreply, S}.

handle_call(terminate, _From, S) ->
    {stop, normal, ok, S};
handle_call(Msg, From, S = #state{}) ->
    io:format("[~s] Unknown call from ~p message ~p~n", [?MODULE, From, Msg]),
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(Reason, _S) ->
    io:format("[~s] Terminate ~p.~n", [?MODULE, Reason]),
    ok.

set_nickname(Nick) ->
    irc_send:raw_unbuf(<< "NICK ", Nick/binary >>),
    ok.

