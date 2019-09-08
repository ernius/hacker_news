%% Unit testing  hacker_stories_api module
-module(hacker_stories_api_tests).

-include("hacker_stories.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

story_1() ->
    #{<<"by">> => <<"pg">>,
      <<"descendants">> => 15,
      <<"id">> => 1,
      <<"type">> => <<"story">>,
      <<"kids">> => [15,234509,487171,454426,454424,454410,82729],
      <<"score">> => 57,
      <<"time">> => 1160418111,
      <<"title">> => <<"Y Combinator">>,
      <<"url">> => <<"http://ycombinator.com">>}.    

%% test fixed known story
check_known_story_test() -> 
    Story = story_1(),
    ?assertMatch(
       {ok, Story},
       hacker_stories_api:get_story(1)).

%% test async top stories
check_top_stories_test() ->
    StoriesNumber = 5,
    hacker_stories_api:get_async_top_stories(StoriesNumber),
    
    Result = receive 
		 {stories, Stories} -> Stories
	     after ?REQUESTS_TIMEOUT*2 -> error
	     end,
    ?assertMatch(StoriesNumber, length(Result)).

%% test hacker news service timeout (httpc:request timeout)
check_timeout_error_test() ->
    meck:new(httpc, [non_strict]),
    meck:expect(httpc, request, fun(_,_,_,_) -> {error, timeout} end),
    Result = hacker_stories_api:get_story(1),
    meck:unload(httpc),
    ?assertMatch(error, Result).

%% test wrong JSON format (jsx:decode error)
check_json_format_error_test() ->
    meck:new(httpc, [non_strict]),
    meck:expect(httpc, request, fun(_,_,_,_) -> 
					{ok,{{"HTTP/1.1",200,"OK"},
					     [{"cache-control","no-cache"},
					      {"connection","keep-alive"},
					      {"date","Sat, 17 Aug 2019 13:50:06 GMT"},
					      {"server","nginx"},
					      {"content-length","186"},
					      {"content-type","application/json; charset=utf-8"},
					      {"access-control-allow-origin","*"},
					      {"strict-transport-security",
					       "max-age=31556926; includeSubDomains; preload"}],
					     "{\"by\":\"pg\""}} %% missing closing } at the end
				end),
    Result = hacker_stories_api:get_story(1),
    meck:unload(httpc),
    ?assertMatch(error, Result).

    
    





