%%%-------------------------------------------------------------------
%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2019 by ernesto <>
%%%-------------------------------------------------------------------
-module(hacker_stories_http_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("hacker_stories.hrl").

-define(HOST, "localhost").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,70}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(hacker_stories),
    {ok, Port} = application:get_env(hacker_stories, rest_port),
    timer:sleep(3*?REQUESTS_TIMEOUT), %% time to fetch top stories
    [{port, Port} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(gun),
    application:stop(inets),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [stories,
     stories_paginated,
     stories_paginated_outofbound,
     stories_websocket
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
stories() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
stories(Config) -> 
    {port, Port} = lists:keyfind(port, 1, Config),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://"++?HOST++":"++integer_to_list(Port)++"/top_stories", []}, [], []),
    jsx:decode(list_to_binary(Body)).

stories_paginated(Config) -> 
    {port, Port} = lists:keyfind(port, 1, Config),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://"++?HOST++":"++integer_to_list(Port)++"/top_stories?page=1", []}, [], []),
    #{ <<"page">> := 1,
       <<"total_pages">> := 5,
       <<"page_size">> := ?PAGINATION_PAGE_SIZE, 
       <<"stories">> := _Stories
     } = jsx:decode(list_to_binary(Body), [return_maps]).

stories_paginated_outofbound(Config) -> 
    {port, Port} = lists:keyfind(port, 1, Config),
    {ok, {{_Version, 404, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://"++?HOST++":"++integer_to_list(Port)++"/top_stories?page=6", []}, [], []),
    #{ <<"message">> := <<"Resource not available">> } = jsx:decode(list_to_binary(Body), [return_maps]).

stories_websocket(Config) ->
    {port, Port} = lists:keyfind(port, 1, Config),
    {ok, ConnPid} = gun:open(?HOST, Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, "/top_stories/websocket"),
    receive
	{gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _Headers} -> ok
    after 1000 -> ok
    end,
    %% Check we receive the stories at connection start, 
    %% for a better testing I should decrease the 5 min fetching period, 
    %% and next loop to verify I receive some number of packets,
    %% with a receive timeout grater than the fetching period
    receive
	{gun_ws, ConnPid, _StreamRef2, {text, Stories}} -> jsx:decode(Stories, [return_maps])
    after 1000 -> ?assert(false)
    end.



