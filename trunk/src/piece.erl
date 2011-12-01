%%% @author Peter Myllykoski <peter@UL30JT>,Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export([init/3]).

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

loop(Piece, PieceIndex, BlockStatus, TorrentPid,PieceSize,LastBlockInfo) ->
	receive
		{unregister, PeerPid} ->
			{Wanted,Downloading,Finished} = BlockStatus,
			case lists:keyfind(PeerPid,2,Downloading) of
				{Block,Pid} ->
					NewBlockStatus = {[Block|Wanted],lists:delete({Block,Pid}, Downloading),Finished};
				false ->
					NewBlockStatus = BlockStatus
			end,
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

construct_piece([],<<Piece/binary>>) ->
	Piece;
construct_piece([{_, <<Block/binary>>}|T], <<Piece/binary>>) ->
	construct_piece(T, <<Piece/binary, Block/binary>>).

create_blocklist(0) ->
	[0];
create_blocklist(NumBlocks) ->
	[NumBlocks|create_blocklist(NumBlocks-1)].

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