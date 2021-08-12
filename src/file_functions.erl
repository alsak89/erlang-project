%%%-------------------------------------------------------------------
%%% @author יובל סער
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. אוג׳ 2021 17:18
%%%-------------------------------------------------------------------
-module(file_functions).
-author("יובל סער").

%% API
-export([split_file/3,merge_file/3]).


-include_lib("kernel/include/file.hrl").
-include_lib("wx/include/wx.hrl").
%%-include_lib("stdlib/include/qlc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Given a full file address, return small chunks of it.
%% Amount of chunks is given by AmountOfPieces.
%% Destination describes the location in which to save the
%% chunked files.
%% to be called like this:
%% file_functions:split_file('tester.txt',3,'C:/final erlang project/testing')
split_file(FileName,AmountOfPieces,SaveOrTable) ->
  %log("Starting the split.~n", []),
  % open file
  case file:open(FileName,[read]) of
    {error,Reason} ->
      erlang:error(Reason);
    {ok,_} ->

      % read its size
      {ok,FileInfo} = file:read_file_info(FileName),
      case element(2,FileInfo) of
        undefined ->
          erlang:error(file_size_undefined);
        FileSize ->

          % calculate each piece's size
          PieceSize = FileSize / AmountOfPieces,
          LeftOverSize = FileSize rem AmountOfPieces,
          %log("File size: ~p Piece size: ~p Leftover size: ~p ~n", [FileSize,PieceSize,LeftOverSize]),

          %% read entire file into binary
          case file:read_file(FileName) of
            {ok,EntireFileInBinary} ->

              %% copy binary to piece files
              copyFromFileToPieces(SaveOrTable,ets:new(binaryTable,[]), FileName,EntireFileInBinary,PieceSize,AmountOfPieces,LeftOverSize,0);
            {error,Reason} ->
              erlang:error(Reason)
          end
      end
  end.

%% @doc Given a list of file chunks, return the original file.
%% The chunks and their amount is given as well as the original
%% file name.
merge_file(OriginalFileName,FilePiecesLocation,Amount) ->

  %% extract original file binaries
  MergedFile = copyFromPiecesToFile(OriginalFileName,<<>>,FilePiecesLocation,Amount,0),

  %% create the new file name
  NewFileName = filename:join([FilePiecesLocation ,
      lists:flatten(io_lib:format("~p",[OriginalFileName]))]),

  % write binary to file
  file:write_file(NewFileName,MergedFile),

  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Given an open file, a destination to put chunks of file in, the
%% size of each chunk, the amount of chunks and the current chunk to
%% be done (starts at 0), copy the file into small chunked pieces of it.
copyFromFileToPieces(SaveOrTable,TableOfBinaries,FileName,FileInBinary, PieceSize, AmountOfPieces, LeftOverSize, CurrentPieceIndex) when AmountOfPieces =:= CurrentPieceIndex ->

  %% END OF THE FUNCTION
  %% if file has a leftover (amount of bytes does not divide by PieceSize without reminder),
  %% create another file and put the reminder in it.
  case LeftOverSize of
    0 -> ok;
    LeftOver ->

      %% take the relevant part from the original file
      PieceFileInBinary = binary:part(FileInBinary,{trunc(PieceSize) * trunc(CurrentPieceIndex), trunc(LeftOver)}),

      %% create the new file name

      case SaveOrTable of
        save ->
          NewFileName = filename:join([
              filename:basename(FileName) ++
              "_part_" ++
              lists:flatten(io_lib:format("~p",[CurrentPieceIndex])) ++
              ".txt"]),
          %% write that part to the new file
          file:write_file(NewFileName, PieceFileInBinary);
        table ->
          NewFileName = filename:join([
              filename:basename(FileName) ++
              "_part_" ++
              lists:flatten(io_lib:format("~p",[CurrentPieceIndex])) ++
              ".txt"]),
          ets:insert(TableOfBinaries,{NewFileName,PieceFileInBinary})
      end
  end,
  TableOfBinaries;

copyFromFileToPieces(SaveOrTable,TableOfBinaries,FileName,FileInBinary, PieceSize, AmountOfPieces, LeftOverSize, CurrentPieceIndex) ->

  %% START OF THE FUNCTION

  %% take the relevant part from the original file
  PieceFileInBinary = binary:part(FileInBinary,{trunc(PieceSize * CurrentPieceIndex), trunc(PieceSize)}),

  case SaveOrTable of
    save ->
      %% create the new file name
      NewFileName = filename:join([
          filename:basename(FileName) ++
          "_part_" ++
          lists:flatten(io_lib:format("~p",[CurrentPieceIndex])) ++
          ".txt"]),
      %% write that part to the new file
      file:write_file(NewFileName, PieceFileInBinary);
    table ->
      %% create the new file name
      NewFileName = filename:join([
          filename:basename(FileName) ++
          "_part_" ++
          lists:flatten(io_lib:format("~p",[CurrentPieceIndex])) ++
          ".txt"]),
      %% insert that part to the table
      ets:insert(TableOfBinaries,{NewFileName,PieceFileInBinary})
  end,
  %% call function recursively
  copyFromFileToPieces(SaveOrTable,TableOfBinaries,FileName,FileInBinary,PieceSize,AmountOfPieces,LeftOverSize, CurrentPieceIndex+1).


%% @doc Given a name of a file, a destination of its chunks,
%% the amount of chunks and the current chunk to
%% be done (starts at 0), copy the chunks into a binary and return it.
copyFromPiecesToFile(_,FileInBinary, _, AmountOfPieces,  CurrentPieceIndex) when AmountOfPieces =:= CurrentPieceIndex ->

  %% END OF THE FUNCTION

  %% return the binary of the new file
  FileInBinary;

copyFromPiecesToFile(OriginalFileName,FileInBinary, Location, AmountOfPieces, CurrentPieceIndex) ->

  %% START OF THE FUNCTION

  %% create the chunk file name
  FileName = filename:join([Location ,
      lists:flatten(io_lib:format("~p",[OriginalFileName])) ++
      "_part_" ++
      lists:flatten(io_lib:format("~p",[CurrentPieceIndex]))]),

  %% read entire chunk file into binary
  case file:read_file(FileName) of
    {ok,EntireFileInBinary} ->

      %% concatenate the chunk file binary with the new file's binary
      NewFileInBinary = <<FileInBinary/binary,EntireFileInBinary/binary>>,

      %% call function recursively
      copyFromPiecesToFile(OriginalFileName,NewFileInBinary, Location, AmountOfPieces,  CurrentPieceIndex + 1);
    {error,Reason} ->
      erlang:error(Reason)
  end.