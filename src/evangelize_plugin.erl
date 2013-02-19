%%% evangelize_plugin spits out a random erlang fact whenever someone mentions erlang
%%% After sending out an erlang fact, it will ignore that users' erlang statements for
%%% a period
-module(evangelize_plugin).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {compiled_regex, quiet_nicks=[]}).
% Time to ignore follow up erlang messages from the same user 
% 30 seconds
-define(WAIT_BETWEEN_FACTS, 30000).

%% gen_server specfic
start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    io:format("[~s] started.~n", [?MODULE]),
    irc_router:add_sub(self()),
    {ok, MP} = re:compile("(^|\s)erlang([\s\.!\?]|$)", [caseless]),
    %{ok, MP} = re:compile("(^|\s)erlang([\s\.|$)", [caseless]),
    {ok, #state{compiled_regex=MP}}.

handle_cast({irc_router, chan_msg, {NickFrom, Channel, Line}}, S = #state{compiled_regex=CRegEx, quiet_nicks=Nicks}) ->
    case {re:run(Line, CRegEx), lists:member(NickFrom, Nicks)} of 
        {{match, _}, false}->
            % Line contained Erlang AND first time nick uttered it within window
            Fact = erlangfacts:getrandom(),
            irc_send:channel(Channel, Fact),
            % Update our list of nicks who we should ignore for a bit
            QNicks = [NickFrom|Nicks],
            io:format("[~s] Silencing ~s for ~p~n", [?MODULE, NickFrom, ?WAIT_BETWEEN_FACTS]),
            erlang:send_after(?WAIT_BETWEEN_FACTS, self(), {unsilence, NickFrom}),
            {noreply, S#state{quiet_nicks=QNicks}};
        _ -> 
            {noreply, S}
    end;
handle_cast(_Msg, S) -> 
    {noreply, S}.

handle_info({unsilence, Nick}, S = #state{quiet_nicks=Nicks}) ->
    io:format("[~s] Unsilencing ~p~n", [?MODULE, Nick]),
    QNicks = lists:delete(Nick, Nicks),
    {noreply, S#state{quiet_nicks=QNicks}};
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
    io:format("[~s] Terminate Reason: ~p.~n", [?MODULE, Reason]),
    ok.

