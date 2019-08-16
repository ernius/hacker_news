%% Module that consumes hacker-news API

-module(hacker_stories_api).

-export([get_story/1, get_top_stories/1]).

-define(API_BASE_URL, "https://hacker-news.firebaseio.com/v0/").
-define(REQUESTS_TIMEOUT, 2000).

-spec get_story(StoryId :: pos_integer()) -> {ok, jsx:json_term()} | error.
get_story(StoryId) ->
    get_url(?API_BASE_URL "item/" ++ integer_to_list(StoryId) ++ ".json").

% Return the available top stories (could be less than StoriesNumber)
-spec get_top_stories(StoriesNumber :: pos_integer()) -> {ok, list(jsx:json_term())} | error.
get_top_stories(StoriesNumber) ->
    try
	{ok, List} = get_url(?API_BASE_URL "topstories.json"),
	TrimList   = lists:sublist(List, StoriesNumber),
	{ok, [ Story || StoryId <- TrimList, {ok, Story} <- [get_story(StoryId)] ]}
    catch
	_:_ -> error
    end.

%% private functions
-spec get_url(URL :: string()) -> {ok, jsx:json_term()} | error.
get_url(URL) ->
    case httpc:request(get, {URL, []}, [{timeout, ?REQUESTS_TIMEOUT}], []) of
    	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> {ok, jsx:decode(list_to_binary(Body), [return_maps])};
    	_                                                      -> error
    end.
