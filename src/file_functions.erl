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
-export([split_file/3]).

-include_lib("kernel/include/file.hrl").
%%-include_lib("stdlib/include/qlc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Given a full file address, return small chunks of it.
%% Amount of chunks is given by AmountOfPieces.
%% Destination describes the location in which to save the
%% chunked files.
split_file(FileName,AmountOfPieces,Destination) ->

  % open file
  case file:open(FileName,[read]) of
    {error,Reason} ->
      erlang:error(Reason);
    {ok,File} ->
      io:format("file opened is ~p ~n",[File]),
      % read its size
      {ok,FileInfo} = file:read_file_info(File),
      io:format("file info is ~p ~n",[FileInfo]),
      case element(2,FileInfo) of
        undefined ->
          erlang:error(file_size_undefined);
        FileSize ->

          % calculate each piece's size
          PieceSize = FileSize / AmountOfPieces,
          copyFromFileToPieces(File,Destination,PieceSize,AmountOfPieces,0)
      end
  end,
  {ok}.





%% @doc Given a list of file chunks, return the original file.
%% The chunks and their amount is given
%%merge_file(ListOfFilePieces,Amount) ->
%%  {ok,File}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Given an open file, a destination to put chunks of file in, the
%% size of each chunk, the amount of chunks and the current chunk to
%% be done (starts at 0), copy the file into small chunked pieces of it.
copyFromFileToPieces(_, _, _, AmountOfPieces, CurrentPieceIndex) when AmountOfPieces =:= CurrentPieceIndex ->
  ok;
copyFromFileToPieces(File, Destination, PieceSize, AmountOfPieces, CurrentPieceIndex) ->
  NewFileName = filename:join([Destination , "part_" ++ lists:flatten(io_lib:format("~p",[CurrentPieceIndex]))]),
  NewFile = file:open(NewFileName,[write]),
  io:format("file opened is now ~p ~n",[File]),
  io:format("now copying part ~p which is of size ~p.~n",[CurrentPieceIndex,PieceSize]),
  case file:copy(File,NewFileName,PieceSize) of
    {ok,BytesTransferred} ->
      io:format("~p bytes were transferred.~n",[BytesTransferred]);
    {error, Reason} ->
      io:format("Error ~p was encountered.~n",[Reason])
  end,
  %file:close(NewFile),
  copyFromFileToPieces(File,Destination,PieceSize,AmountOfPieces,CurrentPieceIndex+1).