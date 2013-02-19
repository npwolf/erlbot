-module(notify_hotword_if_idle_plugin).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAX_CHAN_LINES, 5).
% 5 minutes
-define(EMAIL_AFTER, 300000).
-record(state, {hotwords}).

%% gen_server specfic
start_link() -> 
    io:format("[~s] Start.~n", [?MODULE]),
    gen_server:start_link({local, chanbuffer}, ?MODULE, [], []).

init([]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    {ok, #state{hotwords=orddict:new()}}.


add_hotword(Hotwords, NickFrom, Args) ->
    case re:split(Args, " ", [{parts, 2}, {return, binary}]) of
        [Hotword, Email] -> 
            add_hotword(Hotwords, NickFrom, Hotword, Email);
        _ -> 
            irc_send:priv(NickFrom, <<"Invalid notify_hotword arguments. notify_hotword hotword your@emailaddress.com">>),
            Hotwords
    end.

add_hotword(Hotwords, NickFrom, NewHotword, Email) ->
    LowerHotword = string:to_lower(binary_to_list(NewHotword)),
    LowerEmail = string:to_lower(binary_to_list(Email)),
    case orddict:find(LowerHotword, Hotwords) of
        {ok, HotwordEmailList} ->
            case lists:member(LowerEmail, HotwordEmailList) of
                false -> 
                    UpdatedEList = [LowerEmail|HotwordEmailList];
                true ->
                    irc_send:priv(NickFrom, <<"Hotword and email combination already registered.">>),
                    UpdatedEList = HotwordEmailList
            end;
        error -> 
            UpdatedEList = [LowerEmail]
    end,
    orddict:store(LowerHotword, UpdatedEList, Hotwords),
    Hotwords.


handle_cast({irc_router, cmd_chan, {NickFrom, _Channel, <<"notify_hotword">>, Args}}, S = #state{hotwords=Hotwords}) ->
    io:format("[~s] notify_hotword match ~p~n", [?MODULE, Args]),
    NewHotwords = add_hotword(Hotwords, NickFrom, Args),
    {noreply, S#state{hotwords=NewHotwords}};
handle_cast({irc_router, cmd_chan, {NickFrom, _Channel, <<"help">>, _Args}}, S) ->
    io:format("[~s] help match ~n", [?MODULE]),
    irc_send:priv(NickFrom, <<"Plugin: notify_hotword - Get notified via email if a hotword is sent to a channel and you don't respond to it.  Example: erlbot notify_hotword wolfman nelson.wolf@salesforce.com">>),
    {noreply, S};
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

terminate(normal, #state{}) ->
    io:format("[~s] Terminate.~n", [?MODULE]),
    ok.

