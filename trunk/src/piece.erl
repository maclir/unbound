%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export([init/4,register_peer_process/3]).

init(PieceIndex,PieceLength,_LastPiece,TorrentPid) ->
    <<Piece/bitstring>> = <<0:PieceLength>>,
    BlockSize = 16384,
    NumBlocks = PieceLength div BlockSize,
    PeerPids = [],
	PrivatePeerPids = [],
	BusyPrivatePeerPids =[],
    Wanted = create_blocklist(NumBlocks-1),
    Downloading = [],
    Finished = [],
    loop(Piece,PieceIndex,PeerPids,{Wanted,Downloading,Finished},TorrentPid,PrivatePeerPids,BusyPrivatePeerPids).

loop(<<Piece/bitstring>>, PieceIndex, PeerPids, BlockStatus, TorrentPid, PrivatePeerPids, BusyPrivatePeerPids) ->
    receive
	{register,FromPid} ->
	    NewPeerPids = [FromPid|PeerPids],
	    loop(Piece,PieceIndex,NewPeerPids,BlockStatus,TorrentPid,PrivatePeerPids,BusyPrivatePeerPids);

  	{unregister, PeerPid} ->
	    NewPeerPids = PeerPids -- [PeerPid],
	    NewPrivatePeerPids = PrivatePeerPids -- [PeerPid],
	    {Wanted,Downloading,Finished} = BlockStatus,
	    case lists:keyfind(PeerPid,2,Downloading) of
		{Block,Pid} ->
		    TempBlockStatus = {[Block|Wanted],Downloading--[{Block,Pid}],Finished};
		false ->
		    TempBlockStatus = BlockStatus
	    end,
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,TempBlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,NewPeerPids,NewBlockStatus,TorrentPid,NewPrivatePeerPids,NewBusyPrivatePeerPids);
	
	{block,SenderPid,Offset,Length,BlockBinary} ->
	    {Wanted, Downloading, Finished} = BlockStatus,
	    NewDownloading = Downloading -- [{SenderPid,Offset}],
	    NewFinished = [Offset|Finished],
	    <<HeadBytes:Offset/binary,_Block:Length/binary,Rest/binary>> = Piece,
	    NewPiece = <<HeadBytes/binary,BlockBinary/binary,Rest/binary>>,
	    
	    case Wanted ++ NewDownloading of
		[] ->
		    TorrentPid ! {dowloaded,PieceIndex,NewPiece},
		    receive
			{ok, done} ->
			    ok;
			{error,_Reason} ->
			    ErrorBlockStatus = {NewFinished,[],[]},
			    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,ErrorBlockStatus,PieceIndex),
			    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids)
		    end;
		_ ->
		    TempBlockStatus = {Wanted,NewDownloading,NewFinished},
		    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,TempBlockStatus,PieceIndex),
		    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids)
	    end;
	{connectionsRequest,Pid} ->
	    Pid ! {connection_list,PeerPids},
	    loop(Piece,PieceIndex,PeerPids,BlockStatus,TorrentPid,PrivatePeerPids,BusyPrivatePeerPids);
	{assignedConnections,NewPrivatePeerPids} ->
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,BlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,NewPrivatePeerPids,NewBusyPrivatePeerPids);
	{startDownload} ->
	    {NewBlockStatus,NewBusyPrivatePeerPids} = request_block(PrivatePeerPids -- BusyPrivatePeerPids,BlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,PeerPids,NewBlockStatus,TorrentPid,PrivatePeerPids,NewBusyPrivatePeerPids)
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
 
%% Functions for registering and removing peer processes from peer list

%unregister_peer_process(FromPid,[{Index,ToPid}|T]) ->
%    ToPid ! {unregister,FromPid};

%unregister_peer_process(_FromPid,[]) ->
%    ok.

register_peer_process(FromPid,[{H}|T],PidIndexList) ->
    case lists:keyfind(H,1,PidIndexList) of
	{_Index,ToPid} ->
	    ToPid ! {register,FromPid};
	false ->
	    ok
    end,
    register_peer_process(FromPid,T,PidIndexList);

register_peer_process(_PeerPid,[],_PidIndexList) ->
    ok.


