-module(hacker_story_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, Opts) ->
    try
	SId = cowboy_req:binding(story_id, Req),
	{ok, Story} = hacker_stories_fetch_service:get_story(SId),
	lager:info("Story handler story_id:~p story:~p",[SId, Story]),
	{ok, hacker_stories_web:reply_ok(Req, Story), Opts}
    catch
	_:_ -> {ok, hacker_stories_web:reply_not_available(Req), Opts}       
    end;		 
init(Req, Opts) ->
    {ok, hacker_stories_web:reply_page_not_found(Req), Opts}.
