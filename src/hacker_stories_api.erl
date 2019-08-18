%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc Module that consumes hacker-news API
%%%
%%% @end
%%% Created : 17 Aug 2019 by ernesto <>

-module(hacker_stories_api).

-export([get_story/1, get_top_stories/1]).

-define(API_BASE_URL, "https://hacker-news.firebaseio.com/v0/").
-define(REQUESTS_TIMEOUT, 4000).

%%--------------------------------------------------------------------
%% @doc Return a story with a given Id
%% This call may take REQUESTS_TIMEOUT ms.
%% This 
%% @end
%%--------------------------------------------------------------------

-spec get_story(StoryId :: pos_integer()) -> {ok, jsx:json_term()} | error.
get_story(StoryId) ->
    get_url(?API_BASE_URL "item/" ++ integer_to_list(StoryId) ++ ".json").


%%--------------------------------------------------------------------
%% @doc Return the available top stories (could be less than StoriesNumber, caused by possible get_story errors)
%% As this method ends calling get_url StoriesNumber + 1, and each call may take as much as REQUESTS_TIMEOUT ms,
%% this method may take REQUESTS_TIMEOUT*(StoriesNumber + 1) ms
%% @end
%%--------------------------------------------------------------------
-spec get_top_stories(StoriesNumber :: pos_integer()) -> {ok, list(jsx:json_term())} | error.
get_top_stories(StoriesNumber) ->
    try
	{ok, List} = get_url(?API_BASE_URL "topstories.json"),
	TrimList   = lists:sublist(List, StoriesNumber),
	{ok, [ Story || StoryId <- TrimList, {ok, Story} <- [get_story(StoryId)] ]}
    catch
	% topstories error, get_story errors are just filtered from result list.
	_:_ -> error
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec get_url(URL :: string()) -> {ok, jsx:json_term()} | error.
get_url(URL) ->
    try
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {URL, []}, [{timeout, ?REQUESTS_TIMEOUT}], []),
	{ok, jsx:decode(list_to_binary(Body), [return_maps])}
    catch
	% request timeout or jsx:decode error 
	_:_ -> error
    end.
