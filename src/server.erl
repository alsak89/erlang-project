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
  ets:new(files, [public, named_table, bag]),
  % starts node listener process
  spawn(fun() -> nodesListener(self()) end),
  % start disk monitoring
  application:start(sasl),
  application:start(os_mon),
  {ok, #server_state{}}.

% handles store request
% store record = {my_file.txt, [{my_file_1.txt, client1@IP}, {my_file_2.txt, client2@IP}]}
handle_call({store, StoreRecord}, _From, State = #server_state{}) ->
  io:format("Server received a store record: ~p~n", [StoreRecord]),
  ets:insert(files, StoreRecord),
  {reply, ok, State};

% handles load request
handle_call({load, File}, _From, State = #server_state{}) ->
  io:format("Server received a load request: ~s~n", [File]),
  {reply, lists:nth(1,ets:lookup(files, File)), State};

% handles request for active nodes
handle_call(get_nodes, _From, State = #server_state{}) ->
  io:format("Server received a get_nodes request ~n"),
  {reply, {ets:tab2list(storage_nodes),ets:info(storage_nodes,size)}, State};

% handles request for statistics
handle_call(stats, _From, State = #server_state{}) ->
  io:format("Server received a statistics request ~n"),
  {reply, {ets:tab2list(storage_nodes),ets:info(storage_nodes,size)}, State};

% handles request for files in system
handle_call(get_files, _From, State = #server_state{}) ->
  io:format("Server received a get_files request ~n"),
  {reply, {ets:tab2list(files),ets:info(files,size)}, State};

% handles delete request
handle_call({delete,File}, _From, State = #server_state{}) ->
  io:format("Server received a delete request of file ~s ~n",[File]),

  case ets:lookup(files, File) of
    [{_,FilesToDelete1},{_,FilesToDelete2}] ->
      ets:delete(files,File),
      {reply,{FilesToDelete1,FilesToDelete2}, State};
    [{_,FilesToDelete}] ->
      ets:delete(files,File),
      {reply,{FilesToDelete}, State}
  end;

% handles node_closing request
handle_call({node_closing,Node}, _From, State = #server_state{}) ->
  io:format("Server received a node_closing request from node ~s ~n",[Node]),
  rearrangeFiles(Node),
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

%% WORK IN PROGRESS - FUNCTIONS TO CHANGE SERVER DATABASE WHEN A NODE DIES OR LEAVES SYSTEM
rearrangeFiles(DeadNode) ->
  ets:new(temp,[bag,named_table,public]),
  rearrangeFileTable(files,ets:first(files),temp,DeadNode),
  ets:delete(files),
  ets:rename(temp,files),
  ok.

rearrangeFileTable(_,CurrentFile,_,_) when CurrentFile =:= '$end_of_table' -> ok;
rearrangeFileTable(OldTable,CurrentFile,NewTable,DeadNode) ->
  [{_,FileListWithNodes1},{_,FileListWithNodes2}] = ets:lookup(OldTable,CurrentFile),

  {DeadNodeList1,OtherNodesList1} = separateFileLists(FileListWithNodes1,DeadNode,[],[]),

  NewFileListWithNodes1 = OtherNodesList1 ++ findTwoNodesToSendFilesAndTellThemToSend(DeadNodeList1,FileListWithNodes2,[]),

  {DeadNodeList2,OtherNodesList2} = separateFileLists(FileListWithNodes2,DeadNode,[],[]),

  NewFileListWithNodes2 = OtherNodesList2 ++ findTwoNodesToSendFilesAndTellThemToSend(DeadNodeList2,FileListWithNodes1,[]),

  ets:insert(NewTable,[{CurrentFile,NewFileListWithNodes1},{CurrentFile,NewFileListWithNodes2}]),

  rearrangeFileTable(OldTable,ets:next(OldTable,CurrentFile),NewTable,DeadNode).

  %ListOfFilesAtDeadNode1 = [File || {File, DeadNode} <- FileListWithNodes1],
  %lists:map(fun({_,Node}) -> Node when Node =/= DeadNode end,
%%  case lists:keytake(DeadNode,2,FileListWithNodes1) of
%%    false ->
%%      ok;
%%    {value,{FilePiece,_},FileListWithNodes1WithoutDeadNode} ->
%%      ok
%%  end.

%% separate file list to 2 lists containing files that were on dead node and files that wern't
separateFileLists([],_,DeadNodeList,OtherNodesList) ->
  {DeadNodeList,OtherNodesList};
separateFileLists([H|T],DeadNode,DeadNodeList,OtherNodesList) ->
  case H of
    {_,DeadNode} ->
      separateFileLists(T,DeadNode,DeadNodeList ++ [H],OtherNodesList);
    _ ->
      separateFileLists(T,DeadNode,DeadNodeList,OtherNodesList ++ [H])
  end.

%% given a file and a dead node and a list, find a different node containing that file.
findAnotherNodeWithFile(_,[],_) -> none;
findAnotherNodeWithFile(File,[H|T],DeadNode) ->
  case H of
    {_,DeadNode} ->
      findAnotherNodeWithFile(File,T,DeadNode);
    {File,NewNode} ->
      NewNode;
    _ ->
      findAnotherNodeWithFile(File,T,DeadNode)
  end.

%% given a file and a dead node and a list, find a different node not containing that file.
findAnotherNodeNotWithFile(_,[],_) -> none;
findAnotherNodeNotWithFile(File,[H|T],DeadNode) ->
  case H of
    {_,DeadNode} ->
      findAnotherNodeNotWithFile(File,T,DeadNode);
    {File,_} ->
      findAnotherNodeNotWithFile(File,T,DeadNode);
    {_,NewNode} ->
      NewNode
  end.

%% given a list of dead node tuples and the other file list, find
%% another node which has the dead node file and another node
%% which doesn't, tell them to send the file to each other and
%% return a list with the new file locations.
findTwoNodesToSendFilesAndTellThemToSend([],_, NewFileList) -> NewFileList;
findTwoNodesToSendFilesAndTellThemToSend([H|T],FileListWithNodes, NewFileList) ->
  {File,DeadNode} = H,
  SenderNode = findAnotherNodeWithFile(File,FileListWithNodes,DeadNode),
  RecieverNode = findAnotherNodeNotWithFile(File,FileListWithNodes,DeadNode),
  {other_nodes_listener,SenderNode} ! {server_said_send_file,File,RecieverNode},
  findTwoNodesToSendFilesAndTellThemToSend(T,FileListWithNodes,NewFileList ++ [{File,RecieverNode}]).