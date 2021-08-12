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
  % init ets table to hold storage nodes
  ets:new(storage_nodes, [public, named_table]),
  % init ets table to hold files info
  ets:new(files, [public, named_table]),
  % starts node listener process
  spawn(fun() -> nodesListener(self()) end),
  % start disk monitoring
  application:start(sasl),
  application:start(os_mon),
  {ok, #server_state{}}.

% handles store request
% store record = {my_file.txt, [{my_file_1.txt, client1@IP}, {my_file_2.txt, client2@IP}]}
handle_call({store, StoreRecord}, _From, State = #server_state{}) ->
  io:format("Server received a store record: ~s~n", [StoreRecord]),
  %ets:insert(files, StoreRecord),
  {reply, ok, State};

% handles load request
handle_call({load, File}, _From, State = #server_state{}) ->
  io:format("Server received a load request: ~s~n", [File]),
  {reply, ets:lookup(files, File), State};

% handles request for active nodes
handle_call(get_nodes, _From, State = #server_state{}) ->
  io:format("Server received a get_nodes request ~n"),
  {reply, {ets:tab2list(storage_nodes),ets:info(storage_nodes,size)}, State};

% handles request for statistics
handle_call(stats, _From, State = #server_state{}) ->
  io:format("Server received a statistics request ~n"),
  {reply, {ets:tab2list(storage_nodes),ets:info(storage_nodes,size)}, State}.

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
  net_kernel:stop(),
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
      ets:insert(storage_nodes, {Node, ""}),
      nodesListener(ServerPid);
    {nodedown, Node} ->
      io:format("~p node is down~n", [Node]),
      ets:delete(storage_nodes, Node),
      nodesListener(ServerPid)
  end.