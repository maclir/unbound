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
    loop(Piece,PieceIndex,PeerPidList,{Wanted,Downloading,Finished},TorrentPid).

loop(<<Piece/bitstring>>,PieceIndex,PeerPidList,BlockStatus,TorrentPid) ->
    receive
	{register,FromPid} ->
	    {Wanted,Downloading,Finished} = BlockStatus,
	    NewPeerPidList = [FromPid|PeerPidList],
	    RandomBlock = lists:nth(random:uniform(length(Wanted)),Wanted),
	    NewWanted = Wanted -- [RandomBlock],
	    NewDownloading = [{RandomBlock,FromPid}|Downloading],
	    NewBlockStatus = {NewWanted,NewDownloading,Finished},
	    FromPid ! is_interested,
	    FromPid ! {download_block,self(),PieceIndex,RandomBlock,16384},
	    loop(Piece,PieceIndex,NewPeerPidList,NewBlockStatus,TorrentPid);

	{busy,FromPid,Offset} ->
	    {Wanted,Downloading,Finished} = BlockStatus,
	    NewDownloading = Downloading -- [{Offset,FromPid}],
	    NewWanted = [Offset|Wanted],
	    NewBlockStatus = {NewWanted,NewDownloading,Finished},
	    loop(Piece,PieceIndex,PeerPidList,NewBlockStatus,TorrentPid);
	
  	{unregister, FromPid} ->
	    NewPeerPidList = PeerPidList -- [FromPid],
	    loop(Piece,PieceIndex,NewPeerPidList,BlockStatus,TorrentPid);
	
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
			    loop(Piece,PieceIndex,PeerPidList,
				 ErrorBlockStatus,TorrentPid)
		    end;
		_ ->
		    NewBlockStatus = {Wanted,NewDownloading,NewFinished},
		    loop(Piece,PieceIndex,PeerPidList,NewBlockStatus,TorrentPid)
	    end	
    end.

create_blocklist(0) ->
    [0];

create_blocklist(NumBlocks) ->
    [NumBlocks|create_blocklist(NumBlocks-1)].

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


