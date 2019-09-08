%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc Module that consumes hacker-news API
%%%
%%% @end
%%% Created : 17 Aug 2019 by ernesto <>

-module(hacker_stories_api).

%% Exported API
-export([get_story/1, get_top_stories/1, get_async_top_stories/1]).

%% Internal
-export([get_top_stories_process/2]).

-include("hacker_stories.hrl").

-define(API_BASE_URL, "https://hacker-news.firebaseio.com/v0/").

%%--------------------------------------------------------------------
%% @doc Return a story with a given Id
%% This call may take REQUESTS_TIMEOUT ms.
%% @end
%%--------------------------------------------------------------------
-spec get_story(StoryId :: pos_integer()) -> {ok, jsx:json_term()} | error.
get_story(StoryId) ->
    get_url(story_url(StoryId)).

%%--------------------------------------------------------------------
%% @doc Return StoriesNumber top stories (could be less than StoriesNumber, caused by possible get_story errors)
%% As this method ends calling get_url (StoriesNumber + 1) times, and each url fetch may take as much as REQUESTS_TIMEOUT ms,
%% this method may take REQUESTS_TIMEOUT*(StoriesNumber + 1) ms
%% @end
%%--------------------------------------------------------------------
-spec get_top_stories(StoriesNumber :: pos_integer()) -> {ok, list(jsx:json_term())} | error.
get_top_stories(StoriesNumber) ->
    try
	{ok, List} = get_url(stories_url()),
	TrimList   = lists:sublist(List, StoriesNumber),
	{ok, [ Story || StoryId <- TrimList, {ok, Story} <- [get_story(StoryId)] ]}
    catch
	% topstories error, get_story errors are just filtered from result list.
	Type:Error -> 
	    lager:error("Exception ~p:~p~nStacktrace:~p",[Type, Error, erlang:get_stacktrace()]),
	    error
    end.

%%--------------------------------------------------------------------
%% @doc Return asynchronously the available StoriesNumber top stories (could be less than StoriesNumber, caused by possible errors)
%% A message with no empty fetched stories tuple message {stories, StoriesList} is sent to caller's Pid.
%% If no story could be fetched after 2*REQUESTS_TIMEOUT ms, no message is sent back.
%% @end
%%--------------------------------------------------------------------
-spec get_async_top_stories(StoriesNumber :: pos_integer()) -> ok.
get_async_top_stories(StoriesNumber) ->
    spawn(?MODULE, get_top_stories_process, [StoriesNumber, self()]),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% Hacker News API's URLs.
-spec stories_url() -> string().
stories_url() -> ?API_BASE_URL "topstories.json".

-spec story_url(StoryId :: pos_integer()) -> string().
story_url(StoryId) -> ?API_BASE_URL "item/" ++ integer_to_list(StoryId) ++ ".json".

%% Next functions encapsulates httpc module
-spec get_url(URL :: string()) -> {ok, jsx:json_term()} | error.
get_url(URL) ->
    try
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {URL, []}, [{timeout, ?REQUESTS_TIMEOUT}], []),
	{ok, jsx:decode(list_to_binary(Body), [return_maps])}
    catch
	% request timeout or jsx:decode error 
	Type:Error -> 
	    lager:error("Exception ~p:~p~nStacktrace:~p",[Type, Error, erlang:get_stacktrace()]),
	    error
    end.

-spec get_async_url_wait(URL :: string()) -> {ok, jsx:json_term()} | error.
get_async_url_wait(URL) ->
    try
	{ok, RequestId} 
	    = httpc:request(get, 
			    {URL, []}, 
			    [], 
			    [{sync, false}]),

	receive
	    {http, {RequestId, {_HttpOk, _ResponseHeaders, Body}}} -> 
    		{ok, jsx:decode(Body, [return_maps])}
	after ?REQUESTS_TIMEOUT ->
		lager:info("Timeout waiting url request"),
		error
	end
    catch
    	% jsx:decode error 
    	Type:Error -> 
    	    lager:error("Exception ~p:~p~nStacktrace:~p",[Type, Error, erlang:get_stacktrace()]),
    	    error
    end.

-spec get_async_url(URL :: string()) -> ok | error.
get_async_url(URL) ->
    case httpc:request(get, 
		       {URL, []}, 
		       [], 
		       [{sync, false}]) of
	{ok, _RequestId} -> ok;
	_ -> error
    end.

%% Get asynchronously Number top stories logic
%% May take 2*?REQUESTS_TIMEOUT (fetch top stories and then, in parallel fetch each story)
get_top_stories_process(Number, CallerPid) ->
    try
	{ok, List} = get_async_url_wait(stories_url()),
	TrimList   = lists:sublist(List, Number),
	SuccessfulRequests = length([ StoryId || StoryId <- TrimList, ok == get_async_url(story_url(StoryId)) ]),
	wait_results(CallerPid, SuccessfulRequests, [])
    catch
    	Type:Error -> 
    	    lager:error("Exception ~p:~p~nStacktrace:~p",[Type, Error, erlang:get_stacktrace()])
    end.		

wait_results(CallerPid, 0     , Acc = [_|_]) -> CallerPid ! {stories, Acc};
wait_results(_        , 0     , [] ) -> ok;
wait_results(CallerPid, Number, Acc) ->
    try
	receive
	    {http, {_RequestId, {_HttpOk, _ResponseHeaders, Body}}} -> 
		News = jsx:decode(Body, [return_maps]),
		wait_results(CallerPid, Number - 1, [News | Acc])
	after ?REQUESTS_TIMEOUT ->
		if length(Acc) > 0 -> CallerPid ! {stories, Acc};
		   true            -> ok
		end
	end
    catch
    	% jsx:decode error 
    	Type:Error -> 
	    lager:info("Error ~p:~p", [Type,Error]),
    	    wait_results(CallerPid, Number - 1, Acc)
    end.

