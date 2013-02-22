%%% settings allows you to load settings from defaults or user overrides
%%% First we check ./user_conf/user.conf.erl for the setting
%%% We fallback to ./default_conf/Conf.conf.erl which will have defaults
-module(settings).
-export([get/2]).

%% Get Value of Key in specified configuration (Conf)
%% Currently re-reads file on every call, but these 
%% shouldn't be read too often
get(Conf, Key) ->
    % If user override exists, just return that
    io:format("[~s] Loading ~s from ~s ", [?MODULE, Key, Conf]),
    case get_user(Conf, Key) of
        undefined ->
            % No override, use default
            io:format("Default: "),
            Value = get_default(Conf, Key);
        UserValue -> 
            io:format("Override: "),
            Value = UserValue
    end,
    io:format("~p~n", [Value]),
    Value.

get(File, Conf, Key) when is_list(Conf) ->
    ConfAtom = erlang:list_to_atom(Conf),
    get(File, ConfAtom, Key);
get(File, Conf, Key) ->
    case filelib:is_regular(File) of
        true ->
            {ok, AllSettings} = file:consult(File);
        false ->
            AllSettings = []
    end,
    case proplists:get_value(Conf, AllSettings) of
        undefined -> undefined;
        ConfSettings ->
            case proplists:get_value(Key, ConfSettings) of
                undefined -> 
                    undefined;
                Value -> 
                    Value
            end
    end. 


get_default(Conf, Key) when is_atom(Conf) ->
    ConfList = erlang:atom_to_list(Conf),
    get_default(ConfList, Key);
get_default(Conf, Key) ->
    DefaultConf = "../default_conf/" ++ Conf ++ ".conf.erl",
    case get(DefaultConf, Conf, Key) of
        undefined ->
            Reason = "Could not find key " ++ Key ++ " in conf " ++ Conf,
            throw({error, Reason});
        Value ->
            Value
    end. 

get_user(Conf, Key) ->
    UserConf = "../user_conf/user.conf.erl",
    get(UserConf, Conf, Key).    

