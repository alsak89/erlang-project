-module(gui).

%% API
-export([main/2]).

%-import(file_functions, [split_file/4,merge_file/3]).

-include_lib("wx/include/wx.hrl").

main(NodeName,SavedFilesAddress) ->
  Wx = wx:new(),
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, "gui.xrc"),
  Frame = wxFrame:new(),
  wxXmlResource:loadFrame(Xrc, Frame, Wx, "MainFrame"),
  wxFrame:setMinSize(Frame, { 400, 200 }),
  wxFrame:show(Frame),
  initCallbacks(Frame),
  connectToServer(NodeName),
  ets:new(my_compressed_files,[public, named_table]),
  spawn_link(fun() -> otherNodesListener(SavedFilesAddress) end),
  eventLoop(Frame),
  wx:destroy().

eventLoop(Frame) ->
  receive
    #wx{ event = #wxClose{} } ->
      io:format("Closing GUI...~n"),
      ets:delete(myTable),
      net_kernel:stop(),
      ok;
    Event ->
      io:format("eventLoop: ~p~n", [Event]),
      eventLoop(Frame)
  end.

initCallbacks(Frame) ->
  % subscribe to window close event
  wxFrame:connect(Frame, close_window),
  % grab gui elements
  StoreFileBrowser = wxXmlResource:xrcctrl(Frame, "StoreFileBrowser", wxFilePickerCtrl),
  StoreFileButton = wxXmlResource:xrcctrl(Frame, "StoreFileButton", wxButton),
  LoadFileBrowser = wxXmlResource:xrcctrl(Frame, "LoadFileBrowser", wxFilePickerCtrl),
  LoadFileButton = wxXmlResource:xrcctrl(Frame, "LoadFileButton", wxButton),
  TextEditor = wxXmlResource:xrcctrl(Frame, "TextEditor", wxTextCtrl),
  UpdateFileButton = wxXmlResource:xrcctrl(Frame, "UpdateFileButton", wxButton),
  Logger = wxXmlResource:xrcctrl(Frame, "Logger", wxTextCtrl),
  ClearLoggerButton = wxXmlResource:xrcctrl(Frame, "ClearLoggerButton", wxButton),
  % disable some elements
  wxButton:disable(UpdateFileButton),
  wxTextCtrl:disable(TextEditor),
  wxTextCtrl:disable(Logger),
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
      { userData, {LoadFileBrowser, TextEditor} }]),
  wxTextCtrl:connect(TextEditor, command_text_updated,
    [{callback, fun onTextUpdated/2}, {userData, UpdateFileButton}]),
  wxButton:connect(UpdateFileButton, command_button_clicked,
    [{callback, fun onUpdateFileButtonClick/2}, {userData, {TextEditor, LoadFileBrowser, UpdateFileButton}}]),
  wxButton:connect(ClearLoggerButton, command_button_clicked,
    [{callback, fun onClearLoggerButtonClick/2}, {userData, Logger}]),
  ok.

onStoreFileButtonClick(#wx{ userData = StoreFileBrowser },_) ->
  {ListOfNodes,AmountOfNodes} = gen_server:call({global, server},get_nodes),
  %io:format("Received nodes from server: ~p~n,", Nodes),

  NodesTable = ets:new(nodes_table,[]),
  ets:insert(NodesTable,ListOfNodes),

  log("The table of only nodes: ~p", [NodesTable]),

  ListOfOnlyNodes = keys(NodesTable),

  log("The list of only nodes: ~p", [ListOfOnlyNodes]),

  TableOfBinaries = file_functions:split_file(wxFilePickerCtrl:getPath(StoreFileBrowser),AmountOfNodes,table),

  log("The table of only binaries: ~p", [TableOfBinaries]),

  ListOfBinaries = ets:tab2list(TableOfBinaries),

  log("The list of only binaries: ~p", [ListOfBinaries]),

  ListOfNodesWithFiles = lists:zipwith(fun({FileName,FileBinary},Node) -> {Node,{FileName,FileBinary}} end,ListOfBinaries,ListOfOnlyNodes),

  log("The list of nodes with file names and binaries: ~p", [ListOfNodesWithFiles]),

  %% TableOfNodesWithFiles: Key: node name, Val: {file name, file binary}
  TableOfNodesWithFiles = ets:new(nodes_with_files,[]),
  ets:insert(TableOfNodesWithFiles,ListOfNodesWithFiles),

  log("The table of nodes with file names and binaries: ~p", [TableOfNodesWithFiles]),

  %% send binary files to nodes, return list of which node has which file
  ListOfNodesAndFileNames = sendFiles(TableOfNodesWithFiles,ets:first(TableOfNodesWithFiles),[]),

  log("The list of nodes and files: ~p", [ListOfNodesAndFileNames]),

  gen_server:call({global, server}, {store, wxFilePickerCtrl:getPath(StoreFileBrowser)}),
  log("File stored: ~p", [wxFilePickerCtrl:getPath(StoreFileBrowser)]).

onLoadFileButtonClick(#wx{ userData = {LoadFileBrowser, TextEditor} },_) ->
  %todo: load file remotely
  % read contents of the file
  Path = wxFilePickerCtrl:getPath(LoadFileBrowser),
  gen_server:call({global, server}, {load, Path}),
  log("File loaded: ~s", [Path]),
  {ok, Content} = file:open(Path, read),
  Text = getLines(Content),
  % write to gui
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

onUpdateFileButtonClick(#wx{ userData = {TextEditor, LoadFileBrowser, UpdateFileButton} },_) ->
  File = wxFilePickerCtrl:getPath(LoadFileBrowser),
  wxTextCtrl:saveFile(TextEditor, [{file, File}]),
  wxButton:disable(UpdateFileButton),
  log("Updated file: ~s", [File]).

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
connectToServer(NodeName) ->
  net_kernel:start([NodeName, longnames]),
  net_kernel:connect_node('server@127.0.0.1').

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
sendFiles(_,CurrentNode,ListOfNodesAndFileNames) when CurrentNode =:= '$end_of_table' ->
  ListOfNodesAndFileNames;
sendFiles(TableOfNodesWithFiles,CurrentNode,ListOfNodesAndFileNames) ->
  [{_,{FileName,FileBinary}}] = ets:lookup(TableOfNodesWithFiles,CurrentNode),

  %%  COMPRESSED FILES CODE - CURRENTLY HAS ISSUES
%%  CompressedFileNameStage1 = filename:join([FileName ++ ".tar.gz"]),
%%  CompressedFileNameStage2 = string:split(CompressedFileNameStage1,".txt",all),
%%  CompressedFileName = unicode:characters_to_list(CompressedFileNameStage2),
%%  CompressedFile = erl_tar:create(CompressedFileName,FileBinary,[compressed]),

  FileNameStage1 = string:split(FileName,".txt"),
  FinalFileName = unicode:characters_to_list(FileNameStage1),

  log("Sending file to node",[]),

  %% SEND FILE TO NODE
  {other_nodes_listener,CurrentNode} ! {file_between_nodes,FinalFileName,FileBinary},

  %% call function recursively
  sendFiles(TableOfNodesWithFiles,ets:next(TableOfNodesWithFiles,CurrentNode),ListOfNodesAndFileNames ++ [{FileName,CurrentNode}]).

% listen to other nodes trying to address you by using {other_nodes_listener,nodename@ip} ! {FileName,FileBinary}
% AS OF NOW RECEIVED FILES ARE NOT COMPRESSED
otherNodesListener(SavedFilesAddress) ->
  register(other_nodes_listener,self()),
  otherNodesListenerLoop(SavedFilesAddress).

otherNodesListenerLoop(SavedFilesAddress) ->
  receive
    {file_between_nodes,CompressedFileName,CompressedFile} ->

      %log("Received compressed file ~s with its binary ~p", [CompressedFileName,CompressedFile]),

      ets:insert(my_compressed_files, {CompressedFileName,""}),

      NewCompressedFileName = filename:join([
          SavedFilesAddress ++
          CompressedFileName]),

      %log("Now saving it to location ~s", [NewCompressedFileName]),
      file:write_file(NewCompressedFileName,CompressedFile),
      otherNodesListenerLoop(SavedFilesAddress);
    _ ->
      otherNodesListenerLoop(SavedFilesAddress)
  end.
