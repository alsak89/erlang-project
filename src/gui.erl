-module(gui).

%% API
-export([main/0]).

-include_lib("wx/include/wx.hrl").

main() ->
  Wx = wx:new(),
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, "gui.xrc"),
  Frame = wxFrame:new(),
  wxXmlResource:loadFrame(Xrc, Frame, Wx, "MainFrame"),
  wxFrame:setMinSize(Frame, { 400, 200 }),
  wxFrame:show(Frame),
  initCallbacks(Frame),
  eventLoop(Frame),
  wx:destroy().

eventLoop(Frame) ->
  receive
    #wx{ event = #wxClose{} } ->
      io:format("Closing GUI...~n"),
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
  % put logger in process dict
  put(logger, Logger),
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
  % todo: store the file remotely
  io:format("Selected file to store: ~p~n", [wxFilePickerCtrl:getPath(StoreFileBrowser)]).


onLoadFileButtonClick(#wx{ userData = {LoadFileBrowser, TextEditor} },_) ->
  %todo: load file remotely
  % read contents of the file
  Path = wxFilePickerCtrl:getPath(LoadFileBrowser),
  io:format("File loaded: ~p~n", [Path]),
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
  S = io_lib:format("value = ~p",[42]),
  SS = lists:flatten(S),
  io:format("SS = ~p~n", [SS]).

onClearLoggerButtonClick(#wx{ userData = Logger },_) ->
  wxTextCtrl:clear(Logger).

log(Entry, Args) ->
  List = io_lib:format(Entry,Args),