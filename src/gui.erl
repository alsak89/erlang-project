-module(gui).

%% API

-export([main/2]).
-export([getDiskStats/0]).
-export([getMemoryStats/0]).

-include_lib("wx/include/wx.hrl").

-import(file_functions, [split_file/3,merge_file/4]).

main(ServerName,SavedFilesAddress) ->

  % save file address in ets so listening process get it
  ets:new(general_node_ets,[named_table,public]),
  ets:insert(general_node_ets,{file_address,SavedFilesAddress}),

  % start disk monitoring
  application:start(sasl),
  application:start(os_mon),

  % init gui
  MainFrame = initFrame("MainFrame"),
  wxFrame:show(MainFrame),
  StatsFrame = initFrame("StatsFrame"),
  initMainFrameCallbacks(MainFrame, StatsFrame, SavedFilesAddress),
  initStatFrameCallbacks(StatsFrame),
  connectToServer(ServerName),
  % ets table for transferred files counters
  ets:new(countersTable,[named_table, set, public]),
  ets:insert(countersTable, {received,0}),
  ets:insert(countersTable, {sent,0}),
  spawn_link(fun() -> otherNodesListener(SavedFilesAddress) end),
  eventLoop(MainFrame),
  wx:destroy().

eventLoop(Frame) ->
  receive
    #wx{ event = #wxClose{} } ->
      io:format("Closing GUI...~n"),
      ets:delete(myTable),
      ets:delete(general_node_ets),
      gen_server:call({global, server}, {node_closing, node()}),
      net_kernel:stop(),
      ok;
    Event ->
      io:format("eventLoop: ~p~n", [Event]),
      eventLoop(Frame)
  end.

initMainFrameCallbacks(MainFrame, StatsFrame, SavedFilesAddress) ->
  % subscribe to window close event
  wxFrame:connect(MainFrame, close_window),
  % grab gui elements
  StoreFileBrowser = wxXmlResource:xrcctrl(MainFrame, "StoreFileBrowser", wxFilePickerCtrl),
  StoreFileButton = wxXmlResource:xrcctrl(MainFrame, "StoreFileButton", wxButton),
  %LoadFileBrowser = wxXmlResource:xrcctrl(MainFrame, "LoadFileBrowser", wxFilePickerCtrl),
  LoadFileButton = wxXmlResource:xrcctrl(MainFrame, "LoadFileButton", wxButton),
  TextEditor = wxXmlResource:xrcctrl(MainFrame, "TextEditor", wxTextCtrl),
  UpdateFileButton = wxXmlResource:xrcctrl(MainFrame, "UpdateFileButton", wxButton),
  Logger = wxXmlResource:xrcctrl(MainFrame, "Logger", wxTextCtrl),
  ClearLoggerButton = wxXmlResource:xrcctrl(MainFrame, "ClearLoggerButton", wxButton),
  StatsButton = wxXmlResource:xrcctrl(MainFrame, "StatsButton", wxButton),
  AllFilesButton = wxXmlResource:xrcctrl(MainFrame, "AllFilesButton", wxButton),
  AllFiles = wxXmlResource:xrcctrl(MainFrame, "AllFiles", wxListBox),
  DeleteButton = wxXmlResource:xrcctrl(MainFrame, "DeleteButton", wxButton),
  % disable some elements
  wxButton:disable(UpdateFileButton),
  wxTextCtrl:disable(TextEditor),
  wxTextCtrl:disable(Logger),
  wxButton:disable(DeleteButton),
  wxButton:disable(LoadFileButton),
  % put logger in ets table
  ets:new(myTable, [public, named_table]),
  ets:insert(myTable, {logger, Logger}),
  log("Welcome to BGU Distributed File System!",[]),
  % attach callbacks to elements
  wxButton:connect(StoreFileButton, command_button_clicked,
    [{ callback, fun onStoreFileButtonClick/2 },
      { userData, StoreFileBrowser }]),
  wxButton:connect(LoadFileButton, command_button_clicked,
    [{ callback, fun onLoadFileButtonClick/2 },
      { userData, {AllFiles, TextEditor, SavedFilesAddress} }]),
  wxTextCtrl:connect(TextEditor, command_text_updated,
    [{callback, fun onTextUpdated/2}, {userData, UpdateFileButton}]),
  wxButton:connect(UpdateFileButton, command_button_clicked,
    [{callback, fun onUpdateFileButtonClick/2}, {userData, {TextEditor, SavedFilesAddress, UpdateFileButton, AllFiles}}]),
  wxButton:connect(ClearLoggerButton, command_button_clicked,
    [{callback, fun onClearLoggerButtonClick/2}, {userData, Logger}]),
  wxButton:connect(StatsButton, command_button_clicked,
    [{callback, fun onStatsButtonClick/2}, {userData, StatsFrame}]),
  wxButton:connect(AllFilesButton, command_button_clicked,
    [{callback, fun onAllFilesButtonClick/2}, {userData, AllFiles}]),
  wxButton:connect(DeleteButton, command_button_clicked,
    [{callback, fun onDeleteButtonClick/2}, {userData, AllFiles}]),
  wxListBox:connect(AllFiles, command_listbox_selected,
    [{callback, fun onFileSelection/2}, {userData, {DeleteButton, LoadFileButton}}]),
  ok.

onStoreFileButtonClick(#wx{ userData = StoreFileBrowser },_) ->

  OriginalFileNameWithPath = wxFilePickerCtrl:getPath(StoreFileBrowser),

  storeFileInSystem(OriginalFileNameWithPath).

onLoadFileButtonClick(#wx{ userData = {AllFiles, TextEditor, SavedFilesAddress} },_) ->
  % get selected file name and load it
  SelectedFile = wxListBox:getStringSelection(AllFiles),
  loadFileFromSystemAndSaveLocally(SelectedFile),
  % read content of file and display in editor
  Path = SavedFilesAddress ++ SelectedFile,
  {ok, Content} = file:open(Path, read),
  Text = getLines(Content),
  wxTextCtrl:enable(TextEditor),
  wxTextCtrl:clear(TextEditor),
  wxTextCtrl:changeValue(TextEditor, Text).

getLines(Content) ->
  case io:get_line(Content, "") of
    eof -> [];
    Line -> Line ++ getLines(Content)
  end.

onTextUpdated(#wx{ userData = UpdateFileButton },_) ->
  wxButton:enable(UpdateFileButton).

onUpdateFileButtonClick(#wx{ userData = {TextEditor, SavedFilesAddress, UpdateFileButton, AllFiles} },_) ->
  % construct file path
  SelectedFile = wxListBox:getStringSelection(AllFiles),
  Path = SavedFilesAddress ++ SelectedFile,
  % save locally
  wxTextCtrl:saveFile(TextEditor, [{file, Path}]),
  % store in system
  storeFileInSystem(Path),
  wxButton:disable(UpdateFileButton),
  log("Updated file: ~s", [SelectedFile]).

onClearLoggerButtonClick(#wx{ userData = Logger },_) ->
  wxTextCtrl:clear(Logger).

log(Entry, Args) ->
  % write to logger
  List = io_lib:format(Entry,Args),
  String = lists:flatten(List),
  TupleList = ets:lookup(myTable, logger), % ets returns list of tuples
  Tuple = lists:nth(1, TupleList), % grab 1st (and only) tuple
  WxRef = element(2, Tuple), % grab wx reference from tuple
  wxTextCtrl:appendText(WxRef, String),
  wxTextCtrl:appendText(WxRef, "\n"),
  % write to console
  io:format(Entry, Args),
  io:format("~n").

% connects to the remote server


onStatsButtonClick(#wx{ userData = StatsFrame },_) ->
  % bring up frame
  % todo: StatsFrame is destroyed, so 2nd time will crash
  wxFrame:show(StatsFrame),
  % grab nodes list
  Nodes = wxXmlResource:xrcctrl(StatsFrame, "Nodes", wxListBox),
  updateActiveNodes(Nodes).

% updates active nodes list in stats frame
updateActiveNodes(Nodes) ->
  {ActiveNodes, _} = gen_server:call({global, server}, get_nodes),
  % get list of node names and put them in list box
  NodeNames = [erl_types:atom_to_string(Name) || {Name, _} <- ActiveNodes],
  wxListBox:set(Nodes, NodeNames).


initFrame(Name) ->
  Wx = wx:new(),
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, "gui.xrc"),
  Frame = wxFrame:new(),
  wxXmlResource:loadFrame(Xrc, Frame, Wx, Name),
  wxFrame:setMinSize(Frame, { 400, 200 }),
  Frame.

% functions to get local stats from clients
getDiskStats() ->
  DiskInfo = disksup:get_disk_data(),
  RootPartition = [{Bytes,Percent} || {"/",Bytes,Percent} <- DiskInfo],
  RootPartition.

getMemoryStats() ->
  MemoryInfo = memsup:get_system_memory_data(),
  NeededInfo = lists:sublist(MemoryInfo, 6, 2),
  NeededInfo.

onNodeSelection(#wx{ userData = {Nodes, DiskSpace, Memory, Stats }},_) ->
  wxTextCtrl:clear(Stats),
  SelectedNode = wxListBox:getStringSelection(Nodes),
  TrimmedNode = string:substr(SelectedNode,2, length(SelectedNode)-2),
  % get disk info
  DiskInfo = rpc:call(list_to_atom(TrimmedNode), gui, getDiskStats, []),
  DiskInfoTuple = lists:nth(1, DiskInfo),
  wxGauge:setValue(DiskSpace, element(2, DiskInfoTuple)),
  % get memory info
  MemoryInfo = rpc:call(list_to_atom(TrimmedNode), gui, getMemoryStats, []),
  FreeMemoryTuple = lists:nth(1, MemoryInfo),
  TotalMemoryTuple = lists:nth(2, MemoryInfo),
  FreeMemoryNum = element(2, FreeMemoryTuple),
  TotalMemoryNum = element(2, TotalMemoryTuple),
  UsedMemory = TotalMemoryNum - FreeMemoryNum,
  wxGauge:setValue(Memory, round(UsedMemory/TotalMemoryNum*100)),
  % print file counter
  FileCounters = gen_server:call({global, server}, get_file_counters),
  printStats("Files stored: ~p~nFiles loaded: ~p", [element(1, FileCounters), element(2,FileCounters)], Stats),
  % print files table
  Table = gen_server:call({global, server}, get_files),
  % print to total files
  printStats("--------------------------------------------------", [], Stats),
  printStats("Total files: ~p", [element(2, Table)], Stats),
  printStats("--------------------------------------------------", [], Stats),
  ListOfFileTuples = element(1, Table),
  % print files table
  wxTextCtrl:appendText(Stats, lists:flatten(io_lib:format("~p", [ListOfFileTuples]))).


printStats(String, Args, Stats) ->
  wxTextCtrl:enable(Stats),
  List = io_lib:format(String,Args),
  Text = lists:flatten(List),
  wxTextCtrl:appendText(Stats, Text),
  wxTextCtrl:appendText(Stats,"\n").
  %wxTextCtrl:changeValue(Stats, Text).


initStatFrameCallbacks(StatsFrame) ->
  % get elements
  Nodes = wxXmlResource:xrcctrl(StatsFrame, "Nodes", wxListBox),
  DiskSpace = wxXmlResource:xrcctrl(StatsFrame, "DiskSpace", wxGauge),
  Memory = wxXmlResource:xrcctrl(StatsFrame, "Memory", wxGauge),
  Stats = wxXmlResource:xrcctrl(StatsFrame, "Stats", wxTextCtrl),
  RefreshButton = wxXmlResource:xrcctrl(StatsFrame, "RefreshButton", wxButton),
  CloseButton = wxXmlResource:xrcctrl(StatsFrame, "StatsCloseButton", wxButton),
  % init callbacks
  wxListBox:connect(Nodes, command_listbox_selected,
    [{callback, fun onNodeSelection/2}, {userData, {Nodes, DiskSpace, Memory, Stats}}]),
  wxButton:connect(RefreshButton, command_button_clicked,
    [{callback, fun onRefreshButtonClicked/2}, {userData, Nodes}]),
  wxButton:connect(CloseButton, command_button_clicked,
    [{callback, fun onCloseButtonClicked/2}, {userData, StatsFrame}]).

connectToServer(ServerName) ->
  net_kernel:start([node(), longnames]),
  net_kernel:connect_node(ServerName).


% get a list of all keys from given ets table
keys(TableName) ->
  FirstKey = ets:first(TableName),
  keys(TableName, FirstKey, [FirstKey]).
keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
  Acc;
keys(TableName, CurrentKey, Acc) ->
  NextKey = ets:next(TableName, CurrentKey),
  keys(TableName, NextKey, [NextKey|Acc]).

% send files to nodes
sendFiles(_,CurrentNode,ListOfNodesAndFileNames,SecondListOfNodesAndFileNames) when CurrentNode =:= '$end_of_table' ->

  %% return 2 lists, one for each set of files and nodes
  {ListOfNodesAndFileNames,SecondListOfNodesAndFileNames};
sendFiles(TableOfNodesWithFiles,CurrentNode,ListOfNodesAndFileNames,SecondListOfNodesAndFileNames) ->
  [{_,{FileName,FileBinary}}] = ets:lookup(TableOfNodesWithFiles,CurrentNode),

  %%  COMPRESSED FILES CODE - CURRENTLY HAS ISSUES
%%  CompressedFileNameStage1 = filename:join([FileName ++ ".tar.gz"]),
%%  CompressedFileNameStage2 = string:split(CompressedFileNameStage1,".txt",all),
%%  CompressedFileName = unicode:characters_to_list(CompressedFileNameStage2),
%%  CompressedFile = erl_tar:create(CompressedFileName,FileBinary,[compressed]),

%%  FileNameStage1 = string:split(FileName,".txt"),
%%  FinalFileName = unicode:characters_to_list(FileNameStage1),

  log("Sending file ~s to node ~s",[FileName,CurrentNode]),

  {ok,PID} = ftp:open(CurrentNode),
  ftp:send_bin(PID,FileBinary,FileName),
  ftp:close(PID),
  %% SEND FILE TO NODE 1
  {other_nodes_listener,CurrentNode} ! {save_file,FileName,FileBinary},

  %% SEND FILE TO NODE 2
  case ets:next(TableOfNodesWithFiles,CurrentNode) of

    %% if current node is last node
    '$end_of_table' ->

      %% send file to first node
      SecondNode = ets:first(TableOfNodesWithFiles),
      {other_nodes_listener,SecondNode} ! {save_file,FileName,FileBinary},

      log("Also sending file ~s to node ~s, just in case",[FileName,SecondNode]),

      %% call function recursively
      sendFiles(TableOfNodesWithFiles,
        ets:next(TableOfNodesWithFiles,CurrentNode),
        ListOfNodesAndFileNames ++ [{FileName,CurrentNode}],
        SecondListOfNodesAndFileNames ++ [{FileName,SecondNode}]);

    %% otherwise send file to next node
    SecondNode ->
      {other_nodes_listener,SecondNode} ! {save_file,FileName,FileBinary},

      log("Also sending file ~s to node ~s, just in case",[FileName,SecondNode]),

      %% call function recursively
      sendFiles(TableOfNodesWithFiles,
        ets:next(TableOfNodesWithFiles,CurrentNode),
        ListOfNodesAndFileNames ++ [{FileName,CurrentNode}],
        SecondListOfNodesAndFileNames ++ [{FileName,SecondNode}])
  end.

% request files from nodes
requestFiles(_TableOfFilesWithNodes,CurrentFile) when CurrentFile =:= '$end_of_table' -> ok;
requestFiles(TableOfFilesWithNodes,CurrentFile) ->
  [{_,NodeName}] = ets:lookup(TableOfFilesWithNodes,CurrentFile),

  %% SEND FILE REQUEST TO NODE
  {other_nodes_listener,NodeName} ! {send_me_file,CurrentFile,node()},

  %% call function recursively
  requestFiles(TableOfFilesWithNodes,ets:next(TableOfFilesWithNodes,CurrentFile)).

% delete files from nodes
deleteFiles(_TableOfFilesWithNodes,CurrentFile) when CurrentFile =:= '$end_of_table' -> ok;
deleteFiles(TableOfFilesWithNodes,CurrentFile) ->
  [{_,NodeName}] = ets:lookup(TableOfFilesWithNodes,CurrentFile),

  %% SEND DELETE FILE REQUEST TO NODE
  {other_nodes_listener,NodeName} ! {delete_file,CurrentFile},

  log("sent delete request to node ~s for file ~s",[NodeName,CurrentFile]),

  %% call function recursively
  deleteFiles(TableOfFilesWithNodes,ets:next(TableOfFilesWithNodes,CurrentFile)).

% listen to other nodes trying to address you by using {other_nodes_listener,nodename@ip} ! {FileName,FileBinary}
% AS OF NOW RECEIVED FILES ARE NOT COMPRESSED
otherNodesListener(SavedFilesAddress) ->
  register(other_nodes_listener,self()),
  otherNodesListenerLoop(SavedFilesAddress).

% main loop for otherNodesListener
otherNodesListenerLoop(SavedFilesAddress) ->
  receive

    % save_file is a request from another node to store a file on local node
    {save_file,CompressedFileName,CompressedFile} ->

      % concatenate local address to file name
      NewCompressedFileName = filename:join([
          SavedFilesAddress ++
          CompressedFileName]),

      % save file
      file:write_file(NewCompressedFileName,CompressedFile),

      otherNodesListenerLoop(SavedFilesAddress);

    % send_me_file is a request from another node to send him a file saved on local node
    {send_me_file,FileName,OriginNode} ->

      % concatenate local address to file name
      NewFileName = filename:join([
          SavedFilesAddress ++
          FileName]),

      % retrieve file binary
      {ok,FileBinary} = file:read_file(NewFileName),

      % send file back to requesting client
      {other_nodes_listener,OriginNode} ! {here_is_your_file,FileName,FileBinary},

      otherNodesListenerLoop(SavedFilesAddress);

    % here_is_your_file is a reply from another node containing the requested file
    {here_is_your_file,FileName,FileBinary} ->

      % insert the file into sorted ets called file_pieces
      ets:insert(file_pieces,{FileName,FileBinary}),

      otherNodesListenerLoop(SavedFilesAddress);

    % delete_file is a request from another node to delete file on local node
    {delete_file,FileName} ->

      % concatenate local address to file name
      NewFileName = filename:join([
          SavedFilesAddress ++
          FileName]),

      % delete file from local node
      file:delete(NewFileName),

      otherNodesListenerLoop(SavedFilesAddress);

    %% server_said_send_file is a request from server to send a file to a different node
    %% following a node disconnection(peacefully or not)
    {server_said_send_file,FileName,RecipientNode} ->

      % concatenate local address to file name
      NewFileName = filename:join([
          SavedFilesAddress ++
          FileName]),

      % retrieve file binary
      {ok,FileBinary} = file:read_file(NewFileName),

      % send file to recipient node
      {other_nodes_listener,RecipientNode} ! {save_file,FileName,FileBinary},

      otherNodesListenerLoop(SavedFilesAddress);
    _ ->
      otherNodesListenerLoop(SavedFilesAddress)
  end.

% wait until all file pieces arrive when loading file from other nodes
checkThatAllPiecesAreHere(RequiredSize) ->
  case ets:info(file_pieces,size) of
    RequiredSize ->
      ok;
    _ ->
      log("files not arrived yet, going to sleep for 10 millisecs", []),
      timer:sleep(10),
      checkThatAllPiecesAreHere(RequiredSize)
  end.

deleteFilesHelper(ListOfFilePiecesWithNodes) ->
  %% send request to other nodes to delete file pieces
  TableOfFilePiecesWithNodes = ets:new(temp_ets,[]),
  ets:insert(TableOfFilePiecesWithNodes,ListOfFilePiecesWithNodes),
  deleteFiles(TableOfFilePiecesWithNodes,ets:first(TableOfFilePiecesWithNodes)),
  ets:delete(TableOfFilePiecesWithNodes).

% given a file name without path, delete it from system(assumes it exists)
deleteFileFromSystem(OriginalFileName) ->
  %% request file pieces locations from server for deletion

  case gen_server:call({global, server}, {delete, OriginalFileName}) of

    {ListOfFilePiecesWithNodes1,ListOfFilePiecesWithNodes2} ->
      deleteFilesHelper(ListOfFilePiecesWithNodes1),
      deleteFilesHelper(ListOfFilePiecesWithNodes2);

    {ListOfFilePiecesWithNodes} ->
      deleteFilesHelper(ListOfFilePiecesWithNodes)
  end,
  ok.

% given a file name without path, load it from the system and save it locally
loadFileFromSystemAndSaveLocally(OriginalFileName) ->
  %% request file pieces locations from server
  {_,ListOfFilePiecesWithNodes} = gen_server:call({global, server}, {load, OriginalFileName}),
  log("Requested info for file ~s from server, received: ~p", [OriginalFileName,ListOfFilePiecesWithNodes]),

  %% create new ets to save the file pieces' binaries
  ets:new(file_pieces,[public,ordered_set,{write_concurrency,true},named_table]),

  %% send request to other nodes for file pieces
  TableOfFilePiecesWithNodes = ets:new(temp_ets,[]),
  ets:insert(TableOfFilePiecesWithNodes,ListOfFilePiecesWithNodes),
  AmountOfFiles = ets:info(TableOfFilePiecesWithNodes,size),
  requestFiles(TableOfFilePiecesWithNodes,ets:first(TableOfFilePiecesWithNodes)),
  ets:delete(TableOfFilePiecesWithNodes),

  log("sent requests for files succesfuly, now waiting", []),

  %% wait for all the pieces to arrive
  checkThatAllPiecesAreHere(AmountOfFiles),

  log("all files arrived, constructing back the original.", []),
  %% reconstruct file from pieces
  OriginalFileBinary = file_functions:merge_file(OriginalFileName,file_pieces,AmountOfFiles,table),

  %% delete used ets table
  ets:delete(file_pieces),

  %% reconstruct file name with local address
  [{_,SavedFilesAddress}] = ets:lookup(general_node_ets,file_address),
  FullFileName = filename:join([SavedFilesAddress ++ OriginalFileName]),

  %% save file locally
  file:write_file(FullFileName,OriginalFileBinary),

  log("file ~s succesfuly saved on your system in ~s", [OriginalFileName,FullFileName]),
  ok.

% given a file name with path, store it in the system
storeFileInSystem(OriginalFileNameWithPath) ->
  %% get list of available nodes from server
  {ListOfNodes,AmountOfNodes} = gen_server:call({global, server},get_nodes),

  %% transfom them into table
  NodesTable = ets:new(nodes_table,[]),
  ets:insert(NodesTable,ListOfNodes),

  %% get a list of only the nodes
  ListOfOnlyNodes = keys(NodesTable),

  %% split the stored file into pieces and save them in an ets table
  TableOfBinaries = file_functions:split_file(OriginalFileNameWithPath,AmountOfNodes,table),

  %% get a list from the table
  ListOfBinaries = ets:tab2list(TableOfBinaries),

  log("The list of binaries is ~p, the list of nodes is ~p",[ListOfBinaries,ListOfOnlyNodes]),

  %% create a new list where for each node there is a file tuple(consisting of file name and binary)
  ListOfNodesWithFiles = lists:zipwith(fun({FileName,FileBinary},Node) -> {Node,{FileName,FileBinary}} end,ListOfBinaries,ListOfOnlyNodes),

  %% turn that list into a table
  %% TableOfNodesWithFiles: Key: node name, Val: {file name, file binary}
  TableOfNodesWithFiles = ets:new(nodes_with_files,[]),
  ets:insert(TableOfNodesWithFiles,ListOfNodesWithFiles),

  %% send binary files to nodes, return list of which node has which file ready to be sent to server
  {FirstListOfNodesAndFileNames,SecondListOfNodesAndFileNames} = sendFiles(TableOfNodesWithFiles,ets:first(TableOfNodesWithFiles),[],[]),

  %% get file name without path
  OriginalFileName = filename:basename(OriginalFileNameWithPath),

  %% delete unnecessary ets
  ets:delete(NodesTable),
  ets:delete(TableOfNodesWithFiles),
  ets:delete(TableOfBinaries),

  %% send file name and list of pieces with nodes to server
  gen_server:call({global, server}, {store, [{OriginalFileName,FirstListOfNodesAndFileNames}]}),
  gen_server:call({global, server}, {store, [{OriginalFileName,SecondListOfNodesAndFileNames}]}),
  log("File stored: ~p", [OriginalFileName]).

onRefreshButtonClicked(#wx{ userData = Nodes },_) ->
  updateActiveNodes(Nodes).

onAllFilesButtonClick(#wx{ userData = AllFiles },_) ->
  FilesSummary = gen_server:call({global, server}, get_files),
  FileInfosList = element(1, FilesSummary),
  FileNames = [Name || {Name, _} <- FileInfosList],
  wxListBox:set(AllFiles, lists:usort(FileNames)).

onDeleteButtonClick(#wx{ userData = AllFiles },_) ->
  Selection = wxListBox:getStringSelection(AllFiles),
  deleteFileFromSystem(Selection).


onFileSelection(#wx{ userData = {DeleteButton, LoadFileButton} },_) ->
  wxButton:enable(DeleteButton),
  wxButton:enable(LoadFileButton).

onCloseButtonClicked(#wx{ userData = StatsFrame },_) ->
  wxFrame:hide(StatsFrame).