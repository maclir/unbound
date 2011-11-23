%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/2,init/1]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").

%% =============================================================================
%% Torrent loader function that is responsible for opening the persistent
%% storage and dynamically add all the torrents found into the supervisor.

start_link_loader(Id) ->
	Self = self(),
	spawn_link(?MODULE,init_loader,[{Self,Id}]),
	receive
		{ok,Pid} ->
			{ok,Pid}
		after 100 ->
			{error,time_out}
	end.

init_loader({Pid,Id})->
	io:fwrite("Torrent Loader Started!\n"),
	Pid ! {ok,self()},
	RecordList = [torrent_db:get_torrent_by_id(0)],
	start_torrent(Pid,RecordList,Id).

start_torrent(Pid,[Record|Tail],Id) ->
	InfoHash = info_hash:to_hex(Record#torrent.info_sha),
	StartFunc = {torrent,start_link,[Id,Record]},
	ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	supervisor:start_child(Pid,ChildSpec),
	start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
	ok.



%% =============================================================================
%% Regular torrent functions

start_link(Id,Record) ->
	{ok,spawn_link(torrent,init,[{Id,Record}])}.

init({Id,Record}) ->
    OurBitfield = Record#torrent.info#info.bitfield,
    IndexList = bitfield:to_indexlist(OurBitfield,normal),
    PieceLength = Record#torrent.info#info.piece_length,
    PidIndexList = bind_pid_to_index(IndexList,PieceLength),
    Announce = lists:merge(Record#torrent.announce_list,[Record#torrent.announce]),
    spawn_trackers(Announce,Record#torrent.info_sha,Id),
    loop(Record,#torrent_status{},PidIndexList,[],[],Id).

loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id) ->
    receive
	{peerlist,FromPid,ReceivedPeerList} ->
	    NewTrackerList = [FromPid|TrackerList],
	    NewPeers = ReceivedPeerList -- PeerList,
	    NewPeerList = NewPeers ++ PeerList,
	    spawn_connections(NewPeers,Record#torrent.info_sha,Id),
	    loop(Record,StatusRecord,PidIndexList,NewTrackerList,NewPeerList,Id);
	     
	{bitfield,FromPid,ReceivedBitfield} ->
	    NumPieces = byte_size(Record#torrent.info#info.pieces) div 20,
	    <<Bitfield:NumPieces/bitstring,_Rest/bitstring>> = ReceivedBitfield,
	    PeerIndexList = bitfield:to_indexlist(Bitfield),
	    Intrested = is_intrested(PeerIndexList, PidIndexList),
	    if 
		    Intrested ->
			FromPid ! is_interested;
		    true ->
			FromPid ! not_interested
		end,
	    register_peer_process(FromPid,PeerIndexList,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	{have,FromPid,Index} ->
	    Intrested = is_intrested({Index}, PidIndexList),
	    if 
		Intrested ->
		    FromPid ! is_interested;
		true ->
		    FromPid ! not_interested
	    end,
	    piece:register_peer_process(FromPid,[{Index}],PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
%	{dowloaded,PieceIndex,Data} ->
%	    write_to_file:write(PieceIndex,Data,Record),
	    
	{'EXIT',FromPid,_Reason} ->
	    piece:unregister_peer_process(FromPid,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id)
    end,
    case whereis(assigner) of
	undefined ->
	    register(assigner,spawn(recalculateConnections(PidIndexList)));
	_ ->
	    ok
    end.
spawn_trackers([],_,_) ->
    ok;
spawn_trackers([Announce|AnnounceList],InfoHash,Id) ->
    spawn(self(),Announce,InfoHash,Id),
    spawn_trackers(AnnounceList,InfoHash,Id).

spawn_connections([{Ip,Port}|Rest],InfoHash,Id) ->
    spawn(nettransfer,init,[self(),Ip,Port,InfoHash,Id]),
    spawn_connections(Rest,InfoHash,Id);

spawn_connections([],_InfoHash,_Id) ->
    [].

recalculateConnections(PidIndexList) ->
    ConnectionList = getConnections(PidIndexList,[]),
    SortedConnections = lists:keysort(3,ConnectionList),
    setConnections(SortedConnections,[]).

getConnections([],ResultList) ->
    ResultList;

getConnections([{_Index,Pid}|Tail],ResultList) ->
    Pid ! {connectionsRequest,self()},
    receive
	{connection_list,ConnectionList} ->
	    getConnections(Tail,[{Pid,ConnectionList,length(ConnectionList)}|ResultList])
    after 500 ->
	    getConnections(Tail,ResultList)
    end.

setConnections([],_) ->
    unregister(assigner);

setConnections([{Pid,List,_}|T],Assigned) ->
    Pid ! {assignedConnections,List -- Assigned},
    setConnections(T,Assigned ++ List).

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

bind_pid_to_index([{H}|[]],PieceLength) ->
   [{H,spawn(piece,init,[H,PieceLength,true])}];

bind_pid_to_index([{H}|T],PieceLength) ->
    [{H,spawn(piece,init,[H,PieceLength,false])}|bind_pid_to_index(T,PieceLength)].

is_intrested(PeerIndexList, PidIndexList) ->
	length([Index1 || {Index1} <- PeerIndexList, {Index2, _Pid} <- PidIndexList, Index1 == Index2]) > 0.
