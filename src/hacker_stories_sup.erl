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
                 intensity => 0,
                 period => 1},
    StoriesFetchService = #{id => stories_fetch_service,
			    start => {hacker_stories_fetch_service, start_link, []},
			    restart => permanent,
			    shutdown => 10000,
			    modules => [hacker_stories_fetch_service]},
    HttpService = #{id => http_service,
		    start => {hacker_stories_http_service, start_link, []},
		    restart => permanent,
		    shutdown => 10000,
		    modules => [hacker_stories_http_service]},
    ChildSpecs = [StoriesFetchService, HttpService],
    {ok, {SupFlags, ChildSpecs}}.
