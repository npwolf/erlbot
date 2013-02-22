%%% Default settings always get loaded
%%% Copy tuple's you wish to change to user_conf/user.conf.erl 

{erlbot, [
    % cmd_word - the string the bot sees as its cmd initiator. IE erlbot help
    {cmd_word, "erlbot"},
    % server - IRC server hostname to connect to
    {server, "localhost"},
    % port - IRC server port to connect to 
    {port, 6667}
]}.

