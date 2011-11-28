%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/2,init/1]).
-export([recalculateConnections/2]).
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
    process_flag(trap_exit,true),
    OurBitfield = <<(Record#torrent.info#info.bitfield)/bitstring>>,
    IndexList = bitfield:to_indexlist(OurBitfield,normal),
    PieceLength = Record#torrent.info#info.piece_length,
	Length = Record#torrent.info#info.length,
	LastPieceSize = Length rem PieceLength,
    PidIndexList = bind_pid_to_index(IndexList,PieceLength, LastPieceSize),
    Announce = lists:merge(Record#torrent.announce_list,[Record#torrent.announce]),
    spawn_trackers(Announce,Record#torrent.info_sha,Id),
    loop(Record,#torrent_status{},PidIndexList,[],[],Id).

loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id) ->
    receive
	{choked, _NetPid} ->
	  loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	{im_free, NetPid} ->
	    spawn(torrent,recalculateConnections,[PidIndexList,NetPid]),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	{peer_list,FromPid,ReceivedPeerList} ->
	    io:fwrite("Got Peer List\n"),
	    NewTrackerList = lists:merge(TrackerList,[FromPid]),
	    NewPeers = ReceivedPeerList -- PeerList,
	    NewPeerList = NewPeers ++ PeerList,
	    spawn_connections(NewPeers,Record#torrent.info_sha,Id),
	    loop(Record,StatusRecord,PidIndexList,NewTrackerList,NewPeerList,Id);
	     
	{bitfield,FromPid,ReceivedBitfield} ->
	    io:fwrite("Got Bitfield\n"),
	    NumPieces = byte_size(Record#torrent.info#info.pieces) div 20,
	    <<Bitfield:NumPieces/bitstring,_Rest/bitstring>> = ReceivedBitfield,
	    PeerIndexList = bitfield:to_indexlist(Bitfield,invert),
	    Intrested = is_intrested(PeerIndexList, PidIndexList),
	    if 
		    Intrested ->
		    io:fwrite("Sent Interested\n"),
			FromPid ! is_interested;
		    true ->
		    io:fwrite("Sent not interested\n"),
			FromPid ! not_interested
		end,
	    register_peer_process(FromPid,PeerIndexList,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	{have,FromPid,Index} ->
	    io:fwrite("Got have\n"),
	    Intrested = is_intrested([{Index}], PidIndexList),
	    if 
		Intrested ->
		    io:fwrite("Sent interested (have)\n"),
		    FromPid ! is_interested;
		true ->
		    io:fwrite("Sent not interested (have)\n"),
		    FromPid ! not_interested
	    end,
	    register_peer_process(FromPid,[{Index}],PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	{dowloaded,SenderPid,PieceIndex,Data} ->
				io:fwrite("id: ~p~n", [PieceIndex]),
		Done = bitfield:has_one_zero(Record#torrent.info#info.bitfield),
		case write_to_file:write(PieceIndex,Data,Record,Done) of
			{ok,done} ->
				NewBitField = bitfield:flip_bit(PieceIndex, Record#torrent.info#info.bitfield),
				NewRecord = Record#torrent{info = (Record#torrent.info)#info{bitfield = NewBitField}},
				torrent_db:delete_by_SHA1(Record#torrent.info_sha),
				torrent_db:add(NewRecord),
			lists:keydelete(PieceIndex,1,PidIndexList),
				SenderPid ! {ok, done},
	    		loop(NewRecord,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
			{error, _Reason} ->
				SenderPid ! {error, corrupt_data},
	    		loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id)
		end;
	{'EXIT',FromPid,_Reason} ->
	    io:fwrite("Got EXIT\n"),
	    unregister_peer_process(FromPid,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id)
    end.

spawn_trackers([],_,_) ->
    ok;
spawn_trackers([Announce|AnnounceList],InfoHash,Id) ->
    spawn(tracker,init,[self(),Announce,InfoHash,Id]),
    spawn_trackers(AnnounceList,InfoHash,Id).

spawn_connections([{Ip,Port}|Rest],InfoHash,Id) ->
    spawn_link(nettransfer,init,[self(),Ip,Port,InfoHash,Id]),
    spawn_connections(Rest,InfoHash,Id);

spawn_connections([],_InfoHash,_Id) ->
    [].

recalculateConnections(PidIndexList, NetPid) ->
    ConnectionList = getConnections(PidIndexList,[]),
    SortedConnections = lists:keysort(3,ConnectionList),
    Result = setConnections(SortedConnections,NetPid),
    case Result of
	    downloading ->
		ok;
	    busy ->
		ok;
	    not_needed ->
		NetPid ! {continue}
	end.

getConnections([],ResultList) ->
    ResultList;

getConnections([{_Index,Pid}|Tail],ResultList) ->
    Pid ! {connectionsRequest,self()},
    receive
	{connection_list,ConnectionList} ->
	    getConnections(Tail,[{Pid,ConnectionList,length(ConnectionList)}|ResultList])
	after 50 ->
	    getConnections(Tail,ResultList)
    end.

setConnections([],_) ->
	io:fwrite("haha~n"),
    not_needed;
setConnections([{Pid,List,_}|T],NetPid) ->
	Result = lists:member(NetPid, List),
	if 
		Result ->
    		Pid ! {new_net_pid,self(),NetPid},
			receive
				starting_download ->
					downloading;
				not_needed ->
		    		setConnections(T,NetPid);
				is_busy ->
					%%connection was busy so continue!!
		    		was_busy
			end;
		true ->
    		setConnections(T,NetPid)
    end.

%% Functions for registering and removing peer processes from peer list
unregister_peer_process(FromPid,[{_Index,ToPid}|T]) ->
    ToPid ! {unregister,FromPid},
    unregister_peer_process(FromPid,T);

unregister_peer_process(_FromPid,[]) ->
    ok.

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

bind_pid_to_index([{H}|[]],PieceLength,LastPieceSize) ->
   [{H,spawn(piece,init,[H,PieceLength,self(), LastPieceSize])}];

bind_pid_to_index([{H}|T],PieceLength, LastPieceSize) ->
    [{H,spawn(piece,init,[H,PieceLength,self(), PieceLength])}|bind_pid_to_index(T,PieceLength, LastPieceSize)].

is_intrested(PeerIndexList, PidIndexList) ->
	length([Index1 || {Index1} <- PeerIndexList, {Index2, _Pid} <- PidIndexList, Index1 == Index2]) > 0.
