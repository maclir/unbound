%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export(init/0).

init(IndexNumber,PieceLength,LastPiece) ->
    <<Piece/bitstring>> = <<0:PieceLenght>>,
    BlockSize = 16384,
    NumBlocks = PieceLength / BlockSize,
    PeerPidList = [],
    Wanted = create_block_list(NumBlocks),
    Downloading = [],
    Finished = [],
    loop(Piece,PeerPidList,{Wanted,Downloading,Finished},NumBlocks).

loop(<<Piece/bitstring>>,PieceIndex,PeerPidList,BlockStatus,NumBlocks) ->
    receive
	{register,FromPid} ->
	    {Wanted,Downloading,Finished} = BlockStatus,
	    NewPeerPidList = [Pid|PeerPidList],
	    RandomBlock = lists:nth(random:uniform(lenght(Wanted)),Wanted),
	    NewWanted = Wanted -- [RandomBlock],
	    NewDownloading = [{RandomBlock,FromPid}|Downloading],
	    NewBlockStatus = {NewWanted,NewDownloading,Finished},
	    FromPid ! {download_block,self(),PieceIndex,BlockIndex,16384},
	    loop(Piece,PieceIndex,NewPeerPidList,NewBlockStatus,NumBlocks);
       
	{unregister, FromPid} ->
	    NewPeerPidList = PeerPidList -- [FromPid],
	    loop(Piece,PieceIndex,NewPeerPidList,BlockStatus,NumBlocks)
    end.

create_blocklist(0) ->
    [0];

create_blockList(NumBlocks) ->
    [NumBlocks-1|create_blocklist(NumBlocks-2)].

%% Functions for registering and removing peer processes from peer list

unregister_peer_process(FromPid,[{Index,ToPid}|T]) ->
    ToPid ! {unregister,FromPid};

unregister_peer_process(_FromPid,[]) ->
    ok.

register_peer_process(FromPid,[{H}|T],PidIndexList) ->
    case keyfind(H,1,PidIndexList) of
	{Index,ToPid} ->
	    ToPid ! {register,FromPid};
	false ->
	    ok
    end,
    register_peer_process(PeerPid,T,PidIndexList);

register_peer_process(_PeerPid,[],_PidIndexList) ->
    ok.


