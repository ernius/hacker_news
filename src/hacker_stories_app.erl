%%%-------------------------------------------------------------------
%% @doc hacker_stories public API
%% @end
%%%-------------------------------------------------------------------

-module(hacker_stories_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/top_stories/:story_id", [{story_id,int}], hacker_story_handler, []},
	       {"/top_stories", [], hacker_top_stories_handler, []}]}]),
    {ok, Port} = application:get_env(hacker_stories, rest_port),
    {ok, _} = cowboy:start_clear(http_stories,
    				 [{port, Port}],
    				 #{env => #{dispatch => Dispatch}}
    				),
    hacker_stories_sup:start_link().

stop(_State) ->
    ok.
