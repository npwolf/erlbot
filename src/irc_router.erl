%%% irc_router is an event manager. Raw IRC lines come in, are parsed
%%% and sent to subscribed plugins as Tuple's that are easier to process
-module(irc_router).
-behavior(gen_server).
%% Public 
-export([start_link/1, timestamp/0, timestamp/1, format_irc_line/2, add_sub/1, remove_sub/1]).
%% Don't call these unless you really know what you're doing
-export([recv_raw/1, connected/0, disconnected/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {subscribers=[], cmd_word}).
-include("bot.hrl").

% Public Calls for anyone to call

%% Cmd_word is the string that acts as a command trigger if it is the beginning
%% of a message.  ie Cmd_word = erlbot means this is a command: <nick> erlbot help
start_link(Cmd_word) ->
    gen_server:start_link({local, bot_router}, ?MODULE, [Cmd_word], []).

init([Cmd_word]) ->
    % Comes in as list, convert to binary as I'm dealing
    % with most things internally as binary
    BinCmdWord = list_to_binary(Cmd_word),
    {ok, #state{cmd_word=BinCmdWord}}.

%% Plugins call this as they initialize
%% Subscribed Pids will get sent parsed irc messages
add_sub(Pid) -> 
    erlang:monitor(process, Pid),
    gen_server:call(bot_router, {add_sub, Pid}),
    ok.

%% When the Plugin Pid goes away, we automatically unsubscribe them
%% But if you wanted to unsubscribe manually, you could do it with this
remove_sub(Pid) -> 
    gen_server:call(bot_router, {remove_sub, Pid}),
    ok.

%% Return formatted timestamp.  Utility probably should be moved
timestamp() -> timestamp(now()).
timestamp(Now) -> 
        {_, _, _Micros} = Now, 
        {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
        TS = io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ", 
          [YY, MM, DD, Hour, Min, Sec]),
        list_to_binary(TS).

%% Return formatted irc line for display. Utility probably should be moved
format_irc_line(Nick, Line) ->
    TS = timestamp(),
    <<TS/binary, "<", Nick/binary, "> ", Line/binary>>.


% Private functions start

remove_sub(Subs, Pid) ->
    io:format("[~s] Remove subscriber ~w~n", [?MODULE, Pid]),
    lists:delete(Pid, Subs).

send_subs_msg(Msg) ->
    gen_server:cast(bot_router, {send_subs, Msg}),
    ok.

send_subs_msg([], _Msg) -> ok;
send_subs_msg(Pids, Msg) -> 
    io:format("[~s] send subscribers ~p~n", [?MODULE, Msg]),
    [ gen_server:cast(Pid, Msg)
        ||
        Pid <- Pids],
    ok.

%% Utility function to split how a nick (nick!ident@host)
split_nick(FullNick) ->
    [_FromNick, _IdentHost] = re:split(FullNick, "!", [{parts, 2}, {return, binary}]).

%% Parse irc messages like Python's Twisted http://stackoverflow.com/questions/930700/python-parsing-irc-messages
%% parse_irc(<<":test!~test@test.com PRIVMSG #channel :Hey there">>)
%% #irc_msg{prefix = <<"test!~test@test.com">>, cmd = <<"PRIVMSG">>, args = [<<"#channel">>, <<"Hey there">>]}
parse_irc(<< ":", Rest/binary >>) ->
    [Prefix, NewRest] = re:split(Rest, " ", [{parts, 2}, {return, binary}]),
    parse_irc(Prefix, NewRest);
parse_irc(Rest) ->
    parse_irc(<<>>, Rest).

parse_irc(Prefix, Rest) ->
    case re:run(Rest, " :") of 
        {match, _} ->
            [NewRest, Trailing] = re:split(Rest, " :", [{parts, 2}, {return, binary}]),
            ArgParts = re:split(NewRest, "\s+", [{return, binary}]),
            Args = lists:append(ArgParts, [Trailing]); 
        _ ->
            Args = re:split(Rest, "\s+", [{return, binary}])
    end,
    [Command|FinalArgs] = Args,
    io:format("Prefix '~p' Command '~p' Args '~p'~n", [Prefix, Command, FinalArgs]),
    #irc_msg{prefix=Prefix, cmd=Command, args=FinalArgs}.

% <<" auth me please">> -> [<<"help">>, <<"me please">>]
% <<" auth">> -> [<<"auth">>, <<>>]
% <<"2 auth">> -> [<<"auth">>, <<>>]
get_cmd_args(<<" ", CmdArgs/binary>>) ->
    case re:split(CmdArgs, " ", [{parts, 2}, {return, binary}]) of
        [Cmd, Args] -> [Cmd, Args];
        [Cmd]       -> [Cmd, <<>>]
    end;
% if cmd word had text on end, get rid of it
get_cmd_args(CmdArgs) ->
    case re:split(CmdArgs, " ", [{parts, 2}, {return, binary}]) of
        [_, GoodCmdArgs] -> get_cmd_args(<<" ", GoodCmdArgs/binary>>); 
        % cmd arg just had garbage on end with no cmd
        [_]              -> get_cmd_args(<<" ">>) 
    end.
        
% Private functions END 
% ---------------------


% ------------
% Events START

%% irc_router calls this when it receives a line
recv_raw(Line) ->
    _RawMsg = {irc_router, recv_raw, Line},
    [LineWithoutLineEnd|_] = re:split(Line, "\r\n"),
    MsgRec = parse_irc(LineWithoutLineEnd),
    ParsedMsg = {irc_router, msg_rec, MsgRec},
    gen_server:cast(bot_router, {send_subs, ParsedMsg}).

    
%% Called when connection established
connected() ->
    Msg = {irc_router, connected},
    send_subs_msg(Msg),
    ok.

%% Called when end of motd
ready() ->
    Msg = {irc_router, ready},
    send_subs_msg(Msg),
    ok.

%% Called when connection terminated
disconnected() ->
    Msg = {irc_router, disconnected},
    send_subs_msg(Msg),
    ok.

%% Called for each message in a channel
channel_message(NickFrom, Channel, Line) ->
    Msg = {irc_router, chan_msg, {NickFrom, Channel, Line}},
    send_subs_msg(Msg),
    ok.

%% Called when message received through private message
bot_cmd_priv(NickFrom, Cmd, Args) ->
    Msg = {irc_router, cmd_priv, {NickFrom, Cmd, Args}},
    send_subs_msg(Msg),
    ok.

%% Called when someone starts their channel message out with bot_cmd
%% Example: <nick> erlbot help 
bot_cmd_chan(NickFrom, Channel, Cmd, Args) ->
    Msg = {irc_router, cmd_chan, {NickFrom, Channel, Cmd, Args}},
    send_subs_msg(Msg),
    ok.

% Events END 
% ------------

% ----------------------------
% gen_server mailbox functions 

%% Prefix '<<"nelson!~nelson@localhost">>' Command '<<"PRIVMSG">>' Args '[<<"#erlang">>, <<"Channel messages look like this">>]'
handle_cast({send_subs, {irc_router, msg_rec, 
            #irc_msg{prefix=From, cmd = <<"PRIVMSG">>, args=[<<"#", Chan/binary >>, Line]}}}, 
            S = #state{cmd_word=CmdWord}) ->
    Channel = <<"#", Chan/binary>>,
    [FromNick, _IdentHost] = split_nick(From), 
    case re:split(Line,  <<"^", CmdWord/binary>>, [{parts, 2}, {return, binary}]) of
        [_, CmdArgs] ->
            [Cmd, Args] = get_cmd_args(CmdArgs),
            bot_cmd_chan(FromNick, Channel, Cmd, Args);
        [_]          -> 
            channel_message(FromNick, Channel, Line)
    end,
    {noreply, S};
handle_cast({send_subs, {irc_router, msg_rec, Msg = #irc_msg{cmd = <<"376">>}}}, S = #state{subscribers=SubPids}) ->
    io:format("[~s] End MOTD~n", [?MODULE]),
    ready(),
    send_subs_msg(SubPids, Msg),
    {noreply, S};
handle_cast({send_subs, Msg}, S = #state{subscribers=SubPids}) ->
    send_subs_msg(SubPids, Msg),
    {noreply, S};
handle_cast(Msg, S = #state{}) ->
    io:format("[~s] Unknown cast ~p~n", [?MODULE, Msg]),
    {noreply, S}.

%% A subscriber has died, unsubscribe it from messages
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, S = #state{subscribers=Subs}) ->
    NewSubs = remove_sub(Subs, Pid),
    {noreply, S#state{subscribers=NewSubs}};
handle_info(Msg, S = #state{}) ->
    io:format("[~s] Unknown info ~p~n", [?MODULE, Msg]),
    {noreply, S}.

handle_call({add_sub, Pid}, From, S = #state{subscribers=Subs}) ->
    io:format("[~s] Adding subscriber ~w from ~p~n", [?MODULE, Pid, From]),
    NewState = S#state{subscribers=[Pid|Subs]}, 
    {reply, ok, NewState};
handle_call({remove_sub, Pid}, _From, S = #state{subscribers=Subs}) ->
    NewSubs = remove_sub(Subs, Pid),
    {reply, ok, S#state{subscribers=NewSubs}};
handle_call(Msg, From, S = #state{}) ->
    io:format("[~s] Unknown call from ~p message ~p~n", [?MODULE, From, Msg]),
    {noreply, S}.

terminate(Reason, _S) ->
    io:format("[~s] Terminate. Reason: ~p.~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

