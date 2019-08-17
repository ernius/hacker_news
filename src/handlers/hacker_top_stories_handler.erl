-module(hacker_top_stories_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, Opts) ->
    try
	lager:info("Top Stories handler"),
	{ok, Stories} = hacker_stories_fetch_service:get_stories(),
	{ok, hacker_stories_web:reply_ok(Req, Stories), Opts}
    catch
	_:_ -> {ok, hacker_stories_web:reply_not_available(Req), Opts}       
    end;    
init(Req, Opts) ->
    {ok, hacker_stories_web:reply_page_not_found(Req), Opts}.
