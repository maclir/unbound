%%% @author Peter Myllykoski <peter@UL30JT>,Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export([init/4]).

init(PieceIndex,PieceLength,TorrentPid,PieceSize) ->
    <<Piece/bitstring>> = <<0:PieceLength>>,
    BlockSize = 16384,
    NumBlocks = PieceLength div BlockSize,
    PeerPids = [],
	PrivatePeerPids = [],
	BusyPrivatePeerPids =[],
    Wanted = create_blocklist(NumBlocks-1),
    Downloading = [],
    Finished = [],
    loop(Piece,PieceIndex,PeerPids,{Wanted,Downloading,Finished},TorrentPid,PrivatePeerPids,BusyPrivatePeerPids,PieceSize).

loop(<<Piece/bitstring>>, PieceIndex, PeerPids, BlockStatus, TorrentPid, PrivatePeerPids, BusyPrivatePeerPids,PieceSize) ->
    receive
	{register,FromPid} ->
	    NewPeerPids = [FromPid|PeerPids],
	    loop(Piece,PieceIndex,NewPeerPids,BlockStatus,TorrentPid,PrivatePeerPids,BusyPrivatePeerPids,PieceSize);

  	{unregister, PeerPid} ->
	    NewPeerPids = PeerPids -- [PeerPid],
	    NewPrivatePeerPids = PrivatePeerPids -- [PeerPid],
	    TempBusyPrivatePeerPids = BusyPrivatePeerPids -- [PeerPid],
	    {Wanted,Downloading,Finished} = BlockStatus,
	    case lists:keyfind(PeerPid,2,Downloading) of
		{Block,Pid} ->
		    TempBlockStatus = {[Block|Wanted],Downloading--[{Block,Pid}],Finished};
		false ->
		    TempBlockStatus = BlockStatus
	    end,
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- TempBusyPrivatePeerPids,TempBlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,NewPeerPids,NewBlockStatus,TorrentPid,NewPrivatePeerPids,NewBusyPrivatePeerPids,PieceSize);

	{block,SenderPid,Offset,Length,BlockBinary} ->
	    {Wanted, Downloading, Finished} = BlockStatus,
	    NewDownloading = Downloading -- [{Offset,SenderPid}],
	    NewFinished = [Offset|Finished],
	    <<HeadBytes:Offset/binary,_Block:Length/binary,Rest/binary>> = Piece,
	    NewPiece = <<HeadBytes/binary,BlockBinary/binary,Rest/binary>>,
	    TempBusyPrivatePeerPids = BusyPrivatePeerPids -- [SenderPid],

	    case Wanted ++ NewDownloading of
		[] ->
			<<FinalPiece:PieceSize/binary,_Rest/binary>> = NewPiece,
		    TorrentPid ! {dowloaded,self(),PieceIndex,FinalPiece},
		    receive
			{ok, done} ->
			    ok;
			{error,_Reason} ->
			    ErrorBlockStatus = {NewFinished,[],[]},
			    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- TempBusyPrivatePeerPids,ErrorBlockStatus,PieceIndex),
			    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids,PieceSize)
		    end;
		_ ->
		    TempBlockStatus = {Wanted,NewDownloading,NewFinished},
		    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- TempBusyPrivatePeerPids,TempBlockStatus,PieceIndex),
		    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids,PieceSize)
	    end;
	{connectionsRequest,Pid} ->
	    Pid ! {connection_list,PeerPids},
	    loop(Piece,PieceIndex,PeerPids,BlockStatus,TorrentPid,PrivatePeerPids,BusyPrivatePeerPids,PieceSize);
	{assignedConnections,NewPrivatePeerPids} ->
	    io:fwrite("Private peer pids: ~p\n",[PrivatePeerPids]),
	    io:fwrite("Got private peer pids: ~p\n",[NewPrivatePeerPids]),
	    io:fwrite("Free pids: ~p\n",[NewPrivatePeerPids -- BusyPrivatePeerPids]),
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(NewPrivatePeerPids -- BusyPrivatePeerPids,BlockStatus,PieceIndex),
	    io:fwrite("Busy pids: ~p\n",[NewBusyPrivatePeerPids]),
	    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,NewPrivatePeerPids,NewBusyPrivatePeerPids,PieceSize);
	{startDownload} ->
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,BlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids,PieceSize)
    end.

create_blocklist(0) ->
    [0];

create_blocklist(NumBlocks) ->
    [NumBlocks|create_blocklist(NumBlocks-1)].

request_block(PrivatePids,BlockStatus,PieceIndex) ->
	request_block(PrivatePids,BlockStatus,PieceIndex,[]).

request_block([],BlockStatus,_,BusyPids) ->
    {BlockStatus, BusyPids};

request_block(_,{[],Downloading,Finished},_,BusyPids) ->
    {{[],Downloading,Finished},BusyPids};

request_block([Pid|T],{Wanted,Downloading,Finished},PieceIndex,BusyPids) ->
    Pid ! {download_block,self(),PieceIndex,hd(Wanted),16384},
    receive
	{ok,downloading} ->
	    request_block(T,{tl(Wanted),[{hd(Wanted),Pid}|Downloading],Finished},PieceIndex, [Pid|BusyPids]);
	{busy,_Pid,_Offset} ->
	    request_block(T++[Pid],{Wanted,Downloading,Finished},PieceIndex, BusyPids)
    end.
