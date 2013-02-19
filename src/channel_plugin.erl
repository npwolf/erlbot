%%% channel_plugin handles joining channels
-module(channel_plugin).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {join_channels=[], in_channels=[]}).
-include("bot.hrl").

%% gen_server specfic
start_link(Channels) -> 
    gen_server:start_link(?MODULE, Channels, []).

init(Channels) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    io:format("[~s] started.~n", [?MODULE]),
    irc_router:add_sub(self()),
    BinChans = lists:map(fun(X) -> list_to_binary(X) end, Channels),
    {ok, #state{join_channels=BinChans}}.

%% 366 RPL_ENDOFNAMES to know we've joined the channel
handle_cast({irc_router, msg_rec, #irc_msg{cmd = <<"366">>, args = [_Nick, Channel, _]}}, S = #state{in_channels=InChannels}) ->
    io:format("[~s] Confirmed join ~p~n", [?MODULE, Channel]),
    NewState = S#state{in_channels=[Channel|InChannels]},
    {noreply, NewState};
handle_cast({irc_router, ready}, S = #state{join_channels=Channels}) ->
    join_channels(Channels),
    {noreply, S};
handle_cast({irc_router, disconnected}, S) ->
    {noreply, S#state{in_channels = []}};
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

terminate(Reason, #state{}) ->
    io:format("[~s] Terminate Reason: ~p.~n", [?MODULE, Reason]),
    ok.

join_channels([]) -> ok;
join_channels(Channels) ->
    [JChan|RestChans] = Channels,
    join_channel(JChan),
    join_channels(RestChans).
join_channel(Channel) ->
    io:format("[~s] Joining ~s~n", [?MODULE, Channel]),
    irc_send:raw(<< "JOIN :", Channel/binary>>),
    ok.
