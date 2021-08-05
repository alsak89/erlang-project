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

      % read its size
      FileInfo = file:read_file_info(File),
      case FileInfo#file_info.size of
        undefined ->
          erlang:error(file_size_undefined);
        FileSize ->

          % calculate each piece's size
          PieceSize = FileSize / AmountOfPieces,
          copyFromFileToPieces(File,Destination,PieceSize,AmountOfPieces,0)
      end
  end,
  {ok}.



copyFromFileToPieces(_, _, _, AmountOfPieces, CurrentPieceIndex) when AmountOfPieces =:= CurrentPieceIndex ->
  ok;
copyFromFileToPieces(File, Destination, PieceSize, AmountOfPieces, CurrentPieceIndex) ->
  NewFile = file:open(Destination ++ 'part_' ++ CurrentPieceIndex,[write]),
  file:copy(File,NewFile,PieceSize),
  copyFromFileToPieces(File,Destination,PieceSize,AmountOfPieces,CurrentPieceIndex+1).

%% @doc Given a list of file chunks, return the original file.
%% The chunks and their amount is given
%%merge_file(ListOfFilePieces,Amount) ->
%%  {ok,File}.