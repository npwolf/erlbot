%%% Generic IRC functions that don't require state
-module(irc_utils).
-export([timestamp/0, timestamp/1, format_irc_line/2, bin_format/2]).

%% Return formatted timestamp.  
timestamp() -> timestamp(now()).
timestamp(Now) -> 
        {_, _, _Micros} = Now, 
        {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
        TS = io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ", 
          [YY, MM, DD, Hour, Min, Sec]),
        list_to_binary(TS).

%% Return formatted irc line for display. 
format_irc_line(Nick, Line) ->
    TS = timestamp(),
    <<TS/binary, "<", Nick/binary, "> ", Line/binary>>.

% Alias for iolist_to_binary(io_lib:format(Format, Data))
bin_format(Format, Data) ->
    iolist_to_binary(io_lib:format(Format, Data)).
    
