%%% ping_plugin responds to server PING
-module(ping_plugin).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-include("bot.hrl").

%% gen_server specfic
start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    io:format("[~s] started.~n", [?MODULE]),
    irc_router:add_sub(self()),
    {ok, #state{}}.

handle_cast({irc_router, msg_rec, #irc_msg{cmd = <<"PING">>, args = [Args]}}, S) ->
    io:format("[~s] Matched PING Args ~p~n", [?MODULE, Args]),
    Reply = << "PONG ", Args/binary >>,
    irc_send:raw_unbuf(Reply),
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

terminate(Reason, _S) ->
    io:format("[~s] Terminate Reason: ~p~n", [?MODULE, Reason]),
    ok.

