%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(piece).
-export(init/0).

init() ->
    PeerPidList = [],
    Downloading = [],
    Finished = [],
    loop(PeerPidList,{Downloading,Finished}).

loop(PeerPidList,BlockStatus) ->
    receive
	{register,FromPid} ->
	    NewPeerPidList = [Pid|PeerPidList],
	    loop(NewPeerPidList,BlockStatus);
	{unregister, FromPid} ->
	    NewPeerPidList = PeerPidList -- [FromPid],
	    loop(NewPeerPidList,BlockStatus)
    end.

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


