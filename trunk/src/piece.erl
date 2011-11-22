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
    PeerPidList = [],
    Wanted = create_blocklist(NumBlocks-1),
    Downloading = [],
    Finished = [],
    loop(Piece,PieceIndex,PeerPidList,{Wanted,Downloading,Finished},TorrentPid,[]).

loop(<<Piece/bitstring>>,PieceIndex,PeerPidList,BlockStatus,TorrentPid,PrivatePeerPid,FreePrivatePids) ->
    receive
	{register,FromPid} ->
	    NewPeerPidList = [FromPid|PeerPidList],
	    loop(Piece,PieceIndex,NewPeerPidList,BlockStatus,TorrentPid,PrivatePeerPid,FreePrivatePids);

  	{unregister, PeerPid} ->
	    NewPeerPidList = PeerPidList -- [PeerPid],
	    NewPrivatePeerPid = PrivatePeerPid -- [PeerPid],
	    {Wanted,Downloading,Finished} = BlockStatus,
	    case keyfind(PeerPid,2,Downloading) of
		{Block,Pid} ->
		    TempBlockStatus = {[Block|Wanted],Downloading--[{Block,Pid}],Finished};
		false ->
		    TempBlockStatus = BlockStatus
	    end,
	    {NewBlockStatus,NewFreePrivatePids} = request_block(FreePrivatePids,TempBlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,NewPeerPidList,NewBlockStatus,TorrentPid,PrivatePeerPid,NewFreePrivatePids);
	
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
			    {NewBlockStatus,NewFreePrivatePids} = request_block(FreePrivatePids,ErrorBlockStatus,PieceIndex),
			    loop(Piece,PieceIndex,PeerPidList,NewBlockStatus,TorrentPid,PrivatePeerPid,NewFreePrivatePids)
		    end;
		_ ->
		    TempBlockStatus = {Wanted,NewDownloading,NewFinished},
		    {NewBlockStatus,NewFreePrivatePids} = request_block(FreePrivatePids,TempBlockStatus,PieceIndex),
		    loop(Piece,PieceIndex,PeerPidList,NewBlockStatus,TorrentPid,PrivatePeerPid,NewFreePrivatePids)
	    end;
	{connectionsRequest,Pid} ->
	    Pid ! {connection_list,PeerPidList},
	    loop(Piece,PieceIndex,PeerPidList,BlockStatus,TorrentPid,PrivatePeerPids,FreePrivatePids);
	{assignedConnections,NewPrivatePeerPids} ->
	    TempFreePrivatePids = (NewPrivatePeerPids -- PrivatePids) ++ FreePrivatePids,
	    {NewBlockStatus,NewFreePrivatePids} = request_block(TempPrivatePids,BlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,PeerPidList,NewBlockStatus,TorrentPid,NewPrivatePeerPids,NewFreePrivatePids);
	{startDownload} ->
	    {NewBlockStatus,NewFreePrivate} = request_block(FreePrivatePids,BlockStatus,PieceIndex),
	    loop(Piece,PieceIndex,PeerPidList,BlockStatus,TorrentPid,PrivatePeerPids,NewFreePrivatePids)
    end.

create_blocklist(0) ->
    [0];

create_blocklist(NumBlocks) ->
    [NumBlocks|create_blocklist(NumBlocks-1)].

request_block([],BlockStatus,_)-> 
    {BlockStatus, []};

request_block(Pids,{[],Downloading,Finished},_) ->
    {{[],Downloading,Finished},Pids};

request_block([Pid|T],{Wanted,Downloading,Finished},PieceIndex) ->
    Pid ! {download_block,self(),PieceIndex,hd(Wanted),16384},
    receive
	{ok,downloading} ->
	    request_block(T,{tl(Wanted),[{hd(Wanted),Pid}|Downloading],Finished},PieceIndex);
	{busy,_Pid,_Offset} ->
	    request_block(T++[Pid],{Wanted,Downloading,Finished},PieceIndex)
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


