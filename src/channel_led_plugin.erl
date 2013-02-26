%%% channel_led_plugin was created to toggle LEDs on a raspberry pi
%%% You can actually pass in any fun for the ON and OFF action
%%% It can track multiple start and end events with regular expressions
%%% On the first start action, the on_fun is executed
%%% On the last end action, the off_fun is executed
-module(channel_led_plugin).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([track/2, forget/3, trigger/1]).
-record(state, {on_regex, off_regex, track_nick_keyword=[], on_fun, off_fun, timeout}).

%% gen_server specfic
start_link(Name) -> 
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    %io:format("[~s] started.~n", [?MODULE]),
    Timeout = settings:get(Name, timeout),
    InitAction = settings:get(Name, init_action),
    % led_controller:start_link([{green, 22}, {red, 17}]),
    settings:execute(InitAction),
    ONRegexStr = settings:get(Name, on_regex),
    OFFRegexStr = settings:get(Name, off_regex),
    OnActionStr = settings:get(Name, on_action),
    % Should be a fun
    OnAction = settings:execute(OnActionStr),
    OffActionStr = settings:get(Name, off_action),
    % Should be a fun
    OffAction = settings:execute(OffActionStr),
    irc_router:add_sub(self()),
    {ok, ONRegex} = re:compile(ONRegexStr, [caseless]),
    {ok, OFFRegex} = re:compile(OFFRegexStr, [caseless]), 
    %OnAction = fun() -> led_controller:on(green) end,
    %OffAction = fun() -> led_controller:off(green) end,
    %Timeout = 3600000,
    {ok, #state{on_regex=ONRegex, off_regex=OFFRegex, timeout=Timeout, on_fun=OnAction, off_fun=OffAction}}.

%% When the start action occurs, track the nick and keyword
track(Nick, Keyword) ->
    Pid = self(),
    gen_server:cast(Pid, {track, Nick, Keyword}).

track(Nick, Keyword, [], Timeout) ->
    trigger(on),
    % Init the list so we can call ourselves 
    NickKeywordList = [{{Nick, Keyword}, ignore}],
    track(Nick, Keyword, NickKeywordList, Timeout);
track(Nick, Keyword, NickKeywordList, Timeout) ->
    Pid = self(),
    % After timeout we'll forget this automatically
    {ok, TRef} = timer:apply_after(Timeout, ?MODULE, forget, [Nick, Keyword, Pid]),
    Key = {Nick, Keyword},
    % Cancel the previous timer for last timeout if it exists
    ListNoKey = cancel_timeout(Nick, Keyword, NickKeywordList),
    % Add in new timer
    _ListWithNewKey = [{Key, TRef}|ListNoKey].

cancel_timeout(Nick, Keyword, NickKeywordList) ->
    Key = {Nick, Keyword},
    % Cancel the previous timer for last timeout if it exists
    io:format("[~s] Cancel Key ~p List ~p~n", [?MODULE, Key, NickKeywordList]),
    CancelTRef = proplists:get_value(Key, NickKeywordList),
    case CancelTRef of
        undefined -> 
            io:format("[~s] Key ~p List ~p undefined~n", [?MODULE, Key, NickKeywordList]),
            ok;
        ignore -> ok;
        _ -> timer:cancel(CancelTRef)
    end,
    % Remove key if it exists so we reset the timer ref
    proplists:delete(Key, NickKeywordList).

forget(Nick, Keyword, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {forget, Nick, Keyword});
forget(Nick, Keyword, NickKeywordList) ->
    ListNoKey = cancel_timeout(Nick, Keyword, NickKeywordList),
    % When the list is empty trigger
    io:format("[~s} List ~p~n", [?MODULE, ListNoKey]),
    case ListNoKey of
        [] -> trigger(off);
        _ -> ok 
    end,
    ListNoKey.

trigger(State) ->
    Pid = self(),
    io:format("[~s] Trigger ~p~n", [?MODULE, State]),
    gen_server:cast(Pid, {trigger, State}).

word_match_regex(Line, CRegex) ->
    case re:run(Line, CRegex, [{capture, all_but_first, list}]) of
        {match, [Word|_]} -> Word;
        _ -> nomatch
    end.

check_match(State, Nick, Line, CRegex) ->
    Word = word_match_regex(Line, CRegex),  
    io:format("[~s] State ~p Word ~p~n", [?MODULE, State, Word]),
    case Word of 
        nomatch -> ok;
        _ ->
            case State of
                on -> track(Nick, Word);
                off -> 
                    Pid = self(),
                    forget(Nick, Word, Pid)
            end
    end.

handle_cast({irc_router, chan_msg, {Nick, _Channel, Line}}, S = #state{on_regex=OnRegex, off_regex=OffRegex}) ->
    check_match(on, Nick, Line, OnRegex),
    check_match(off, Nick, Line, OffRegex),
    {noreply, S};
handle_cast({track, Nick, Keyword}, S = #state{track_nick_keyword=NickKeywordList, timeout=Timeout}) ->
    NewList = track(Nick, Keyword, NickKeywordList, Timeout),
    {noreply, S#state{track_nick_keyword=NewList}};
handle_cast({forget, Nick, Keyword}, S = #state{track_nick_keyword=NickKeywordList}) ->
    NewList = forget(Nick, Keyword, NickKeywordList),
    {noreply, S#state{track_nick_keyword=NewList}};
handle_cast({trigger, State}, S = #state{on_fun=OnAction, off_fun=OffAction}) ->
    case State of
        on -> OnAction();
        off -> OffAction()
    end,
    {noreply, S};
handle_cast(_Msg, S) -> 
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
    io:format("[~s] Terminate Reason: ~p.~n", [?MODULE, Reason]),
    ok.

