%%% irc_send has functions that should be used to send all messages
%%% Will eventually modify to have a send queue so we don't get 
%%% flooded off the server by sending too much too quickly
-module(irc_send).
%% Public 
-export([raw/1, raw_unterminated/1, channel/2, priv/2]).

%% Public Interface 
% Use this to send server messages. Includes \r\n 
raw(Msg) -> 
    send(Msg).

%% Use this to send server message and skip send buffer. Includes \r\n 
%% Note send buffer not implemented yet
raw_unbuf(Msg) -> 
    send(Msg).

% Rare to use.  Does not terminate with \r\n
raw_unterminated(Msg) ->
    send_raw(Msg).

% Send channel a msg
channel(Channel, Msg) -> 
    send(<<"PRIVMSG ", Channel/binary, " :", Msg/binary>>).

% Send private message to Nick
priv(NickTo, Msg) -> 
    send(<<"PRIVMSG ", NickTo/binary, " :", Msg/binary>>).

%% Private interface
send(Msg) -> 
    send_raw(<<Msg/binary, "\r\n">>).

send_raw(Msg) ->
    bot_conn:send(Msg).


