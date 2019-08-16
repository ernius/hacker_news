-module(hacker_stories_api).

-export([get_story/1, get_top_stories/1]).

-define(API_BASE_URL, "https://hacker-news.firebaseio.com/v0/").

-spec get_story(StoryId :: pos_integer()) -> map().
get_story(StoryId) ->
    get_url(?API_BASE_URL "item/" ++ integer_to_list(StoryId) ++ ".json").

-spec get_top_stories(StoriesNumber :: pos_integer()) -> list(map()).
get_top_stories(StoriesNumber) ->
    List     = get_url(?API_BASE_URL "topstories.json"),
    TrimList = lists:sublist(List, StoriesNumber),
    [get_story(StoryId) || StoryId <- TrimList].

%% private functions
-spec get_url(URL :: string()) -> jsx:json_term().
get_url(URL) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
    jsx:decode(list_to_binary(Body), [return_maps]).
