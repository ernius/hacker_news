%%%-------------------------------------------------------------------
%% @doc hacker_stories public API
%% @end
%%%-------------------------------------------------------------------

-module(hacker_stories_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start cowboy application, it has its own supervisor
    Dispatch = cowboy_router:compile([
        {'_', routes()}]),
    {ok, Port} = application:get_env(hacker_stories, rest_port),
    {ok, _} = cowboy:start_clear(http_stories,
    				 [{port, Port}],
    				 #{env => #{dispatch => Dispatch}}
    				),

    hacker_stories_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Cowboy routes
routes() ->
    [{"/top_stories/:story_id", [{story_id,int}], hacker_story_handler, []},
     {"/top_stories", [], hacker_top_stories_handler, []},
     {"/top_stories/websocket", [], hacker_stories_websocket_handler, []}
    ].
