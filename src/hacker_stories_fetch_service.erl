%%%-------------------------------------------------------------------
%%% @author ernesto <>
%%% @copyright (C) 2019, ernesto
%%% @doc
%%% This service subscribe/unsubscribe pids throw async messages.
%%% Every 5 minutes fetch top stories, loads them in the ETS, and notify all subscribed Pids. 
%%% Stores loaded in ETS are available throw API get_stories/0 and get_story/1.
%%% 
%%% @end
%%% Created : 16 Aug 2019 by ernesto <>
%%%-------------------------------------------------------------------
-module(hacker_stories_fetch_service).

-behaviour(gen_server).

%% API
-export([start_link/0, get_story/1, get_stories/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(FETCH_PERIOD, 5000).%TODO: 5*60*1000).
-define(ETS_TABLE_NAME, top_stories_table).
-define(ETS_TABLE_KEY, top_stories_table).
-define(N_TOP_STORIES, 2). %TODO: 50).

-record(state, {timer_ref :: erlang:reference(), web_sockets_pids :: [pid()]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Get story with given Id. Read from ETS.
%% @end
%%--------------------------------------------------------------------
-spec get_story(StoryId :: pos_integer()) -> {ok, map()} | none.
get_story(StoryId) ->
    case ets:lookup(?ETS_TABLE_NAME, ?ETS_TABLE_KEY) of
	[{?ETS_TABLE_KEY, Stories}] -> 
	    lists:foldl(fun (Story = #{<<"id">> := Id}, Acc) -> 
				if StoryId =:= Id -> {ok, Story};
				   true           -> Acc
				end;
			    (_                         , Acc) -> Acc 
			end,
			none,
			Stories);
	[] -> none
    end.

%%--------------------------------------------------------------------
%% @doc Get top stories. Read from ETS.
%% @end
%%--------------------------------------------------------------------
-spec get_stories() -> {ok, list(map())} | none.
get_stories() ->
    case ets:lookup(?ETS_TABLE_NAME, ?ETS_TABLE_KEY) of
	[{?ETS_TABLE_KEY, Stories}] -> {ok, Stories};
	[] -> none
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    lager:info("Fetch top stories service started!"),
    ets:new(?ETS_TABLE_NAME, [set, named_table]),
    self() ! fetch,
    {ok, #state{ web_sockets_pids = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch top stories when message arrives.
%% A message is received every ?FETCH_PERIOD ms, fetching top stories and store them in ETS.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(fetch, State = #state{ web_sockets_pids = Pids}) ->
    % only update ets if stories could been fetched
    case hacker_stories_api:get_top_stories(?N_TOP_STORIES) of
	{ok, Stories} -> 
	    lager:info("Fetched ~p stories~n",[length(Stories)]),
	    ets:insert(?ETS_TABLE_NAME, {?ETS_TABLE_KEY, Stories}),
	    [Pid ! {stories, Stories} || Pid <- Pids];
	error -> 
	    lager:error("Stories could not be fetched"),
	    ok
    end,
    TRef = erlang:send_after(?FETCH_PERIOD, self(), fetch),
    {noreply, State#state{timer_ref = TRef}};
handle_info({subscribe, Pid}, State = #state{ web_sockets_pids = Pids}) ->
    {noreply, State#state{ web_sockets_pids = [Pid | Pids] }};
handle_info({unsubscribe, Pid}, State = #state{ web_sockets_pids = Pids}) ->
    {noreply, State#state{ web_sockets_pids = lists:delete(Pid, Pids) }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Cancels timer
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, #state{timer_ref = TRef, web_sockets_pids = Pids}) ->
    [Pid ! terminate || Pid <- Pids],
    timer:cancel(TRef).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
