
-module(erlbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    CmdWord = settings:get("erlbot", cmd_word),
    IrcServer = settings:get("erlbot", server),
    IrcPort = settings:get("erlbot", port),
    % This start order is important
    {ok, { {one_for_one, 5, 10}, [
       ?CHILD(irc_router, worker, CmdWord),
       ?CHILD_SUP(plugin_sup),
       ?CHILD(bot_conn, worker, [IrcServer, IrcPort])
            ]} }.


