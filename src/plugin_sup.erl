%%% plugin_sup is a supervisor for our plugins
-module(plugin_sup).

-behaviour(supervisor).

-export([start_link/0, load_plugins/0]).
-export([init/1, get_plugins/0]).

start_link() ->
    Strategy = {one_for_one, 6, 60},
    supervisor:start_link({local,?MODULE}, ?MODULE, Strategy).

init({RestartStrategy, MaxRestart, MaxTime}) ->
    Plugins = get_plugins(),
    {ok, {{RestartStrategy, MaxRestart, MaxTime}, Plugins}}.

%% Call load_plugins to load plugins defined in get_plugins()
%% You can modify get_plugins definition, compile, and call load_plugins
%% and only newly added plugins will be loaded
load_plugins() ->
    % Fully qualify get_plugins so if we get latest version
    Plugins = plugin_sup:get_plugins(),
    case supervisor:check_childspecs(Plugins) of
        {error, Error} ->
            io:format("[~s] Error in plugins definition: ~p~n", [?MODULE, Error]),
            {error, Error};
        ok ->
            load_plugins(Plugins),
            ok
    end.
    
get_plugins() ->
     [
      {nick_plugin_id,
       {nick_plugin, start_link, ["erlbot"]},
       permanent, 1000, worker, [nick_plugin]},
      {channel_plugin_id,
          {channel_plugin, start_link, [["#erlang", "#bots"]]},
       permanent, 1000, worker, [channel_plugin]},
      {ping_plugin_id,
       {ping_plugin, start_link, []},
       permanent, 1000, worker, [ping_plugin]},
      {evangelize_plugin_id,
       {evangelize_plugin, start_link, []},
       permanent, 1000, worker, [evangelize_plugin]}
    ].

load_plugins([]) -> ok;
load_plugins([Plugin|PluginsToLoad]) ->
    {PluginId, _, _, _, _, _} = Plugin,
    io:format("[~s] Start loading ~p~n", [?MODULE, PluginId]),
    case supervisor:start_child(?MODULE, Plugin) of
        {error, already_present} ->
            io:format("[~s] Skip loading ~p is already present. Duplicate id?~n", [?MODULE, PluginId]);
        {error, {already_started, _Child}} ->
            io:format("[~s] Skip loading ~p, already loaded~n", [?MODULE, PluginId]);
        {ok, _} ->
            io:format("[~s] Done loading ~p.~n", [?MODULE, PluginId]);
        {ok, _, _} ->
            io:format("[~s] Done loading ~p.~n", [?MODULE, PluginId])
    end,
    load_plugins(PluginsToLoad).


