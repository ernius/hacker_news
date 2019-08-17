%% Unit testing  hacker_stories_api module
-module(hacker_stories_api_SUITE).

-include_lib("eunit/include/eunit.hrl").

% test fixed known story
check_known_story_test() -> 
    ?assertMatch(
       {ok, #{<<"by">> := <<"pg">>,
	      <<"descendants">> := 15,
	      <<"id">> := 1,
	      <<"type">> := <<"story">>,
	      <<"kids">> := [15,234509,487171,454426,454424,454410,82729],
	      <<"score">> := 57,<<"time">> := 1160418111,
	      <<"title">> := <<"Y Combinator">>,
	      <<"url">> := <<"http://ycombinator.com">>}},
       hacker_stories_api:get_story(1)).

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
					     "{\"by\":\"pg\""}}
				end),
    Result = hacker_stories_api:get_story(1),
    meck:unload(httpc),
    ?assertMatch(error, Result).

    
    





