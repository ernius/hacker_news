-module(hacker_top_stories_handler).

-include("hacker_stories.hrl").

-export([init/2]).

init(Req=#{method := <<"GET">>}, Opts) ->
    try
	lager:info("Top Stories handler"),
	QsVals = cowboy_req:parse_qs(Req),
	case lists:keyfind(<<"page">>, 1, QsVals) of
	    {_, Page} ->	
		lager:info("Page requested: ~p",[Page]),
		{ok, PagesNumber, Stories} = hacker_stories_fetch_service:get_paginated_stories(list_to_integer(binary_to_list(Page))),
		{ok, 
		 hacker_stories_web:reply_ok(Req, 
					     #{ <<"page">> => Page, 
						<<"total_pages">> => PagesNumber, 
						<<"page_size">> => ?PAGINATION_PAGE_SIZE, 
						<<"stories">> => Stories }), 
		 Opts};
	    false ->
		lager:info("Requested all top stories"),
		{ok, Stories} = hacker_stories_fetch_service:get_stories(),
		{ok, hacker_stories_web:reply_ok(Req, Stories), Opts}
	end
    catch
	error:badarg ->
	    lager:error("Bad query parameter"),
            {ok, hacker_stories_web:reply_bad_request(Req), Opts}; 	
	Type:Error -> 
	    lager:error("Exception ~p:~p~nStacktrace:~p",[Type, Error, erlang:get_stacktrace()]),
	    {ok, hacker_stories_web:reply_not_available(Req), Opts}       
    end;    
init(Req, Opts) ->
    {ok, hacker_stories_web:reply_page_not_found(Req), Opts}.
