%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc
%%% Webservice handler
%%% @end
%%% Created : 17 Aug 2019 by ernesto <>

-module(hacker_stories_webservice_handler).

%% cowboy_websocket callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

init(Req=#{method := <<"GET">>}, State) ->
    {cowboy_websocket, Req, State};
init(Req, Opts) ->
    {ok, hacker_stories_web:reply_page_not_found(Req), Opts}.

%%%===================================================================
%%% Websocket protocol callbacks
%%%===================================================================

% websocket process initialization
websocket_init(State) ->
    {reply, {text, <<"Hello webserice!">>}, State}.

% message from client
websocket_handle(_Frame, State) ->
    {ok, State}.

% erlang messages
websocket_info({stories, Stories}, State) ->
    {reply, {text, jsx:encode(Stories)}, State}.




