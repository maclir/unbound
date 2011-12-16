%%% @author Peter Myllykoski <peter@UL30JT>,Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export([init/3]).

%%----------------------------------------------------------------------
%% Function: init/3
%% Purpose:  initializes the piece index and length
%% Args:     PieceIndex(integer),TorrentPid(pid),PieceSize(integer)
%%----------------------------------------------------------------------
init(PieceIndex,TorrentPid,PieceSize) ->
	BlockSize = 16384,
	TempNumBlocks = PieceSize div BlockSize,
	TempLastBlockSize = PieceSize rem BlockSize,
	if
		(TempLastBlockSize /= 0) ->
			NumBlocks = TempNumBlocks + 1,
			LastBlockSize = TempLastBlockSize;
		true ->
			NumBlocks = TempNumBlocks,
			LastBlockSize = 16384
	end,
	BlockStatus = {create_blocklist(NumBlocks-1), [], []},
	LastBlockInfo = {NumBlocks - 1 ,LastBlockSize},
	loop([],PieceIndex,BlockStatus,TorrentPid,PieceSize,LastBlockInfo).

%%----------------------------------------------------------------------
%% Function:  loop/6
%% Purpose:   it handles everything related to the pieces of the torrent file.
%% Args:      Piece(list),PieceIndex(integer),BlockStatus(tuple),TorrentPid(pid),PieceSize(integer),LastBlockInfo{tuple}
%%----------------------------------------------------------------------
loop(Piece, PieceIndex, BlockStatus, TorrentPid,PieceSize,LastBlockInfo) ->
	receive
		{unregister, PeerPid} ->
			NewBlockStatus = unregister_pid(PeerPid, BlockStatus),
			loop(Piece,PieceIndex,NewBlockStatus,TorrentPid,PieceSize,LastBlockInfo);

		{block,SenderPid,Offset,Length,BlockBinary} ->
			{Wanted, Downloading, Finished} = BlockStatus,
			OurOffset = (Offset div 16384),
			NewDownloading = Downloading -- [{OurOffset,SenderPid}],
			NewFinished = [OurOffset|Finished],
			<<FinalBlock:Length/binary, _Rest/binary>> = BlockBinary,
			NewPiece = [{OurOffset, FinalBlock}|Piece],
			case Wanted ++ NewDownloading of
				[] ->
					<<FinalPiece/binary>> = construct_piece(lists:keysort(1,NewPiece), <<>>),
					TorrentPid ! {dowloaded,self(),PieceIndex,FinalPiece},
					receive
						{ok, done} ->
							ok;
						{error,_Reason} ->
							NewBlockStatus = {NewFinished,[],[]},
							loop([],PieceIndex,NewBlockStatus,TorrentPid,PieceSize,LastBlockInfo)
					end;
				_ ->
					NewBlockStatus = {Wanted,NewDownloading,NewFinished},
					loop(NewPiece,PieceIndex,NewBlockStatus,TorrentPid,PieceSize,LastBlockInfo)
			end;
		{new_net_pid,FromPid,NetPid} ->
			{Wanted, _Downloading, _Finished} = BlockStatus,
			case Wanted of
				[] ->
					FromPid ! not_needed,
					NewBlockStatus = BlockStatus;
				_ ->
					FromPid ! needed,
					NewBlockStatus = request_block(BlockStatus,PieceIndex,NetPid,LastBlockInfo)
			end,
			loop(Piece,PieceIndex,NewBlockStatus,TorrentPid,PieceSize,LastBlockInfo)
	end.
%%----------------------------------------------------------------------
%% Function:    unregister_pid/2
%% Purpose:     it unregisters the tcp processes
%% Args:        PeerPid(pid),{Wanted,Downloading,Finished}(tuple)
%%----------------------------------------------------------------------
unregister_pid(PeerPid, {Wanted, Downloading, Finished}) ->
	case lists:keyfind(PeerPid,2,Downloading) of
		{Block,Pid} ->
			unregister_pid(PeerPid, {[Block|Wanted],lists:delete({Block,Pid}, Downloading),Finished});
		false ->
			{Wanted, Downloading, Finished}
	end.

%%----------------------------------------------------------------------
%% Function:  construct_piece/2
%% Purpose:
%% Args:
%% Returns:
%%----------------------------------------------------------------------
construct_piece([],<<Piece/binary>>) ->
	Piece;
construct_piece([{_, <<Block/binary>>}|T], <<Piece/binary>>) ->
	construct_piece(T, <<Piece/binary, Block/binary>>).

%%----------------------------------------------------------------------
%% Function:  create_blocklist/0
%% Purpose:
%% Returns:
%%----------------------------------------------------------------------
create_blocklist(0) ->
	[0];
create_blocklist(NumBlocks) ->
	[NumBlocks|create_blocklist(NumBlocks-1)].

%%----------------------------------------------------------------------
%% Function:  request_block/4
%% Purpose:
%% Args:      {Wanted,Downloading,Finished}(tuple),PieceIndex(integer),NetPid(pid),{LastBlockInfo,LastBlockSize}(tuple)
%% Returns:
%%----------------------------------------------------------------------
request_block({Wanted,Downloading,Finished},PieceIndex,NetPid,{LastBlockIndex,LastBlockSize}) ->
	[HeadWanted|TailWanted] = Wanted,
	if
		(HeadWanted == LastBlockIndex) ->
			BlockSize = LastBlockSize;
		true ->
			BlockSize = 16384
	end,
	NetPid ! {download_block,self(),PieceIndex,HeadWanted*16384,BlockSize},
	{TailWanted,[{HeadWanted, NetPid}|Downloading],Finished}.
