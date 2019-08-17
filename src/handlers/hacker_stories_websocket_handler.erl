%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc
%%% Webservice handler
%%% @end
%%% Created : 17 Aug 2019 by ernesto <>

-module(hacker_stories_websocket_handler).

%% cowboy_websocket callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
	 terminate/3
	]).

init(Req=#{method := <<"GET">>}, State) ->
    {cowboy_websocket, Req, State};
init(Req, Opts) ->
    {ok, hacker_stories_web:reply_page_not_found(Req), Opts}.

%%%===================================================================
%%% Websocket protocol callbacks
%%%===================================================================

% websocket process initialization
websocket_init(State) ->
    hacker_stories_fetch_service:subscribe(self()),
    case hacker_stories_fetch_service:get_stories() of
	{ok, Stories} -> {reply, {text, jsx:encode(Stories)}, State};
        none          -> {ok, State}
    end.

% message from client
websocket_handle(_Frame, State) ->
    {ok, State}.

% erlang messages
websocket_info({stories, Stories}, State) ->
    {reply, {text, jsx:encode(Stories)}, State}.

terminate(_Reason, _Req, _State) ->
    hacker_stories_fetch_service:unsubscribe(self()),
    ok.



