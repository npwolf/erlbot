%%% irc_send has functions that should be used to send all messages
%%% All messages should be sent via this and will get the appropriate
%%% delay to prevent flooding
-module(irc_send).
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Public 
-export([raw/1, raw_unbuf/1, channel/2, priv/2]).
% How long to pause between messages to prevent flooding off
-define(MSG_DELAY, 250).

start_link() ->
    gen_server:start_link({local, send_queue}, ?MODULE, [], []).

init([]) ->
    {ok, none}.

%% Public Interface 

raw(Msg) when is_list(Msg) =:= false -> 
    raw([Msg]);
raw(Msgs) -> 
    FormattedMsgs = lists:map(fun(Msg) -> 
                <<Msg/binary, "\r\n">>
                end,
            Msgs),
    enqueue(FormattedMsgs).

%% Send a raw message avoiding delay. 
%% Good for responding to ping, bad for other stuff
raw_unbuf(Msg) when is_list(Msg) =:= false -> 
    raw([Msg]);
raw_unbuf([]) -> ok;
raw_unbuf([Msg|Msgs]) -> 
    FormattedMsg = <<Msg/binary, "\r\n">>,
    bot_conn:send(FormattedMsg),
    raw_unbuf(Msgs).

% Send channel a msg
% Msg can be list or single binary string
channel(Channel, Msg) -> 
    generic_send(Channel, Msg).

% Send private message to Nick
priv(NickTo, Msg) -> 
    generic_send(NickTo, Msg).

%% Private interface

% Used for both private and channel messages
generic_send(Where, Msg) when is_list(Msg) =:= false -> 
    generic_send(Where, [Msg]);
generic_send(Where, Msgs) -> 
    % Wrap what we want to send in irc format
    FormattedMsgs = lists:map(fun(Msg) -> 
                <<"PRIVMSG ", Where/binary, " :", Msg/binary, "\r\n">>
                end,
            Msgs),
    enqueue(FormattedMsgs).

% At this point it must be a list
enqueue(Msgs) when is_list(Msgs) =:= true ->
    gen_server:cast(send_queue, {send_msg, Msgs}).

send_delayed(Msg) when is_list(Msg) =:= false ->
    send_delayed([Msg]);
send_delayed([]) -> ok;
send_delayed([Msg|Rest]) ->
    bot_conn:send(Msg),
    timer:sleep(?MSG_DELAY),
    send_delayed(Rest).

handle_cast({send_msg, Msg}, S) ->
    send_delayed(Msg),
    {noreply, S};
handle_cast(Msg, S) ->
    io:format("[~w] Unexpected message: ~p~n", [?MODULE, Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    io:format("[~w] Unexpected message: ~p~n", [?MODULE, Msg]),
    {noreply, S}.

handle_call(terminate, _From, S) ->
    {stop, normal, ok, S}.

%% On termination we'll quickly send a random fact as our quit message
terminate(Reason, _S) ->
    io:format("[~s] Terminate Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

