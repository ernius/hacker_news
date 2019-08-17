%%%-------------------------------------------------------------------
%% @doc hacker_stories public API
%% @end
%%%-------------------------------------------------------------------

-module(hacker_stories_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hacker_stories_sup:start_link().

stop(_State) ->
    ok.
