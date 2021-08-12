-module(gui).

%% API
-export([main/0]).
-export([getDiskStats/0]).
-export([getMemoryStats/0]).

-include_lib("wx/include/wx.hrl").

main() ->
  % start disk monitoring
  application:start(sasl),
  application:start(os_mon),
  % init gui
  MainFrame = initFrame("MainFrame"),
  wxFrame:show(MainFrame),
  StatsFrame = initFrame("StatsFrame"),
  initMainFrameCallbacks(MainFrame, StatsFrame),
  initStatFrameCallbacks(StatsFrame),
  connectToServer(),
  eventLoop(MainFrame),
  wx:destroy().

eventLoop(Frame) ->
  receive
    #wx{ event = #wxClose{} } ->
      io:format("Closing GUI...~n"),
      ets:delete(myTable),
      ok;
    Event ->
      io:format("eventLoop: ~p~n", [Event]),
      eventLoop(Frame)
  end.

initMainFrameCallbacks(MainFrame, StatsFrame) ->
  % subscribe to window close event
  wxFrame:connect(MainFrame, close_window),
  % grab gui elements
  StoreFileBrowser = wxXmlResource:xrcctrl(MainFrame, "StoreFileBrowser", wxFilePickerCtrl),
  StoreFileButton = wxXmlResource:xrcctrl(MainFrame, "StoreFileButton", wxButton),
  LoadFileBrowser = wxXmlResource:xrcctrl(MainFrame, "LoadFileBrowser", wxFilePickerCtrl),
  LoadFileButton = wxXmlResource:xrcctrl(MainFrame, "LoadFileButton", wxButton),
  TextEditor = wxXmlResource:xrcctrl(MainFrame, "TextEditor", wxTextCtrl),
  UpdateFileButton = wxXmlResource:xrcctrl(MainFrame, "UpdateFileButton", wxButton),
  Logger = wxXmlResource:xrcctrl(MainFrame, "Logger", wxTextCtrl),
  ClearLoggerButton = wxXmlResource:xrcctrl(MainFrame, "ClearLoggerButton", wxButton),
  StatsButton = wxXmlResource:xrcctrl(MainFrame, "StatsButton", wxButton),
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
  wxButton:connect(StatsButton, command_button_clicked,
    [{callback, fun onStatsButtonClick/2}, {userData, StatsFrame}]),
  ok.

onStoreFileButtonClick(#wx{ userData = StoreFileBrowser },_) ->
  Nodes = gen_server:call({global, server},get_nodes),
  io:format("Received nodes from server: ~p~n,", Nodes),

  % todo: store the file remotely
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
connectToServer() ->
  net_kernel:start(['gui@127.0.0.1', longnames]),
  net_kernel:connect_node('server@127.0.0.1').

onStatsButtonClick(#wx{ userData = StatsFrame },_) ->
  % bring up frame
  % todo: StatsFrame is destroyed, so 2nd time will crash
  wxFrame:show(StatsFrame),
  % grab nodes list
  Nodes = wxXmlResource:xrcctrl(StatsFrame, "Nodes", wxListBox),
  ActiveNodes = gen_server:call({global, server}, get_nodes),
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

getDiskStats() ->
  DiskInfo = disksup:get_disk_data(),
  RootPartition = [{Bytes,Percent} || {"/",Bytes,Percent} <- DiskInfo],
  RootPartition.

getMemoryStats() ->
  MemoryInfo = memsup:get_system_memory_data(),
  NeededInfo = lists:sublist(MemoryInfo, 6, 2),
  NeededInfo.


onNodeSelection(#wx{ userData = {Nodes, DiskSpace, Memory }},_) ->
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
  io:format("Used mem: ~p~n", [UsedMemory]),
  wxGauge:setValue(Memory, round(UsedMemory/TotalMemoryNum*100)).

initStatFrameCallbacks(StatsFrame) ->
  % get elements
  Nodes = wxXmlResource:xrcctrl(StatsFrame, "Nodes", wxListBox),
  DiskSpace = wxXmlResource:xrcctrl(StatsFrame, "DiskSpace", wxGauge),
  Memory = wxXmlResource:xrcctrl(StatsFrame, "Memory", wxGauge),
  Stats = wxXmlResource:xrcctrl(StatsFrame, "Stats", wxTextCtrl),
  % init callbacks
  wxListBox:connect(Nodes, command_listbox_selected,
    [{callback, fun onNodeSelection/2}, {userData, {Nodes, DiskSpace, Memory}}]).


