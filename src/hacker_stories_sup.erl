%%%-------------------------------------------------------------------
%% @doc hacker_stories top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hacker_stories_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1, % if more than 1 restart occur in 5 seconds the supervisor terminates and all its childs
                 period    => 5},

    StoriesFetchService = #{id => stories_fetch_service,
			    start => {hacker_stories_fetch_service, start_link, []},
			    restart => permanent, % child is always restarted by supevisor
			    shutdown => 10000,    % time supervisor waits after exit(Child, shutdown) is sent
			    modules => [hacker_stories_fetch_service]},

    ChildSpecs = [StoriesFetchService],
    {ok, {SupFlags, ChildSpecs}}.
