%%%-------------------------------------------------------------------
%%% @author יובל סער
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. אוג׳ 2021 17:11
%%%-------------------------------------------------------------------
-module(server).
-author("יובל סער").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% init of the server
init([]) ->
  io:format("Initiating server...~n"),
  % start as distributed node
  net_kernel:start(['server@127.0.0.1', longnames]),
  % subscribe to nodes monitoring
  %net_kernel:monitor_nodes(true),
  % init ets table to hold storage nodes
  ets:new(storage_nodes, [public, named_table]),
  % starts node listener process
  spawn(fun() -> nodesListener(self()) end),
  {ok, #server_state{}}.

% handles store request
handle_call({store, File}, _From, State = #server_state{}) ->
  io:format("Server received a store request: ~s~n", [File]),
  {reply, ok, State};

% handles load request
handle_call({load, File}, _From, State = #server_state{}) ->
  io:format("Server received a load request: ~s~n", [File]),
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_cast(_Request, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_info(_Info, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #server_state{}) -> term()).
terminate(_Reason, _State = #server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #server_state{},
    Extra :: term()) ->
  {ok, NewState :: #server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% listens to nodes up/down events
% updates node status in ets table
nodesListener(ServerPid) ->
  net_kernel:monitor_nodes(true),
  receive
    {nodeup, Node} ->
      io:format("~p node is up~n", [Node]),
      ets:insert(storage_nodes, {Node, up}),
      nodesListener(ServerPid);
    {nodedown, Node} ->
      io:format("~p node is down~n", [Node]),
      ets:insert(storage_nodes, {Node, down}),
      nodesListener(ServerPid)
  end.