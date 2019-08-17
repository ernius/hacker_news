%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc
%%% Abstracted cowboy functionalities
%%% @end
%%% Created : 17 Aug 2019 by ernesto <>

-module(hacker_stories_web).

-export([reply_page_not_found/1, reply_ok/2, reply_not_available/1]).

-spec reply_ok(cowboy_req:req(), term()) -> cowboy_req:req().
reply_ok(Req, Content) ->
    reply(Req, 200, Content).

-spec reply_not_available(cowboy_req:req()) -> cowboy_req:req().
reply_not_available(Req) ->
    reply(Req, 404, #{<<"message">> => <<"Resource not available">>}).

-spec reply_page_not_found(cowboy_req:req()) -> cowboy_req:req().
reply_page_not_found(Req) ->
  reply(Req, 404, #{<<"message">> => <<"Not found">>}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec reply(cowboy_req:req(), non_neg_integer(), term()) -> cowboy_req:req().
reply(Req, StatusCode, Content) ->
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Content), Req).


