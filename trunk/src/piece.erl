%%% @author Peter Myllykoski <peter@UL30JT>,Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export([init/4]).

init(PieceIndex,PieceLength,TorrentPid,PieceSize) ->
    <<Piece/binary>> = <<0:(PieceLength*8)>>,
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
	PeerPids = [],
    Wanted = create_blocklist(NumBlocks-1),
    Downloading = [],
    Finished = [],
    loop(Piece,PieceIndex,PeerPids,{Wanted,Downloading,Finished},TorrentPid,PieceSize,{NumBlocks - 1 ,LastBlockSize}).

loop(<<Piece/binary>>, PieceIndex, PeerPids, BlockStatus, TorrentPid,PieceSize,LastBlockSize) ->
    receive
	{register,FromPid} ->
	    NewPeerPids = [FromPid|PeerPids],
	    loop(Piece,PieceIndex,NewPeerPids,BlockStatus,TorrentPid,PieceSize,LastBlockSize);

  	{unregister, PeerPid} ->
	    NewPeerPids = PeerPids -- [PeerPid],
	    {Wanted,Downloading,Finished} = BlockStatus,
	    case lists:keyfind(PeerPid,2,Downloading) of
		{Block,Pid} ->
		    NewBlockStatus = {[Block|Wanted],Downloading--[{Block,Pid}],Finished};
		false ->
		    NewBlockStatus = BlockStatus
	    end,
	    loop(Piece,PieceIndex,NewPeerPids,NewBlockStatus,TorrentPid,PieceSize,LastBlockSize);

	{block,SenderPid,Offset,Length,BlockBinary} ->
	    {Wanted, Downloading, Finished} = BlockStatus,
		OurOffset = (Offset div 16384),
	    NewDownloading = Downloading -- [{OurOffset,SenderPid}],
	    NewFinished = [OurOffset|Finished],
	    <<HeadBytes:Offset/binary,_Block:Length/binary,Rest/binary>> = Piece,
	    NewPiece = <<HeadBytes/binary,BlockBinary/binary,Rest/binary>>,
	    case Wanted ++ NewDownloading of
		[] ->
		    <<FinalPiece:PieceSize/binary,_Rest/binary>> = NewPiece,
		    TorrentPid ! {dowloaded,self(),PieceIndex,FinalPiece},
		    receive
			{ok, done} ->
			    ok;
			{error,_Reason} ->
			    NewBlockStatus = {NewFinished,[],[]},
			    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PieceSize,LastBlockSize)
		    end;
		_ ->
		    NewBlockStatus = {Wanted,NewDownloading,NewFinished},
		    loop(NewPiece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PieceSize,LastBlockSize)
	    end;
	{connectionsRequest,Pid} ->
	    Pid ! {connection_list,PeerPids},
	    loop(Piece,PieceIndex,PeerPids,BlockStatus,TorrentPid,PieceSize,LastBlockSize);
	{new_net_pid,AssignerPid,NetPid} ->
		{Wanted, _Downloading, _Finished} = BlockStatus,
		case Wanted of
			[] ->
				AssignerPid ! not_needed,
				NewBlockStatus = BlockStatus;
			_ ->
			    {Result, NewBlockStatus} = request_block(BlockStatus,PieceIndex,NetPid,LastBlockSize),
				AssignerPid ! Result
		end,
	    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PieceSize,LastBlockSize)
    end.

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
    receive
	{ok,downloading} ->
	    {starting_download,{TailWanted,[{HeadWanted, NetPid}|Downloading],Finished}};
	{busy,_Pid,_Offset} ->
	    {is_busy,{Wanted,Downloading,Finished}}
    end.
