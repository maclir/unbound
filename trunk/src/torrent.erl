%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/2,init/1]).
-export([recalculateConnections/3]).
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
    TempLastPieceSize = Length rem PieceLength,
    case TempLastPieceSize of
	0 ->
	    LastPieceSize = PieceLength;
	_ ->
	    LastPieceSize = TempLastPieceSize
    end,
    PidIndexList = bind_pid_to_index(IndexList,PieceLength, LastPieceSize),
    AnnounceList = lists:flatten(Record#torrent.announce_list) -- [Record#torrent.announce],
    Announce = AnnounceList ++ [Record#torrent.announce],
    spawn_trackers(Announce,Record#torrent.info_sha,Id),
    loop(Record,#torrent_status{},PidIndexList,[],[],Id).

loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id) ->
	receive
	    {get_statistics,Pid} ->
		Pid ! {statistics,0,0,0},
		    loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	    {choked, _NetPid} ->
		loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	    {im_free, NetPid} ->
		spawn(torrent,recalculateConnections,[PidIndexList,NetPid,0]),
		loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	    {peer_list,FromPid,ReceivedPeerList} ->
		io:fwrite("Got Peer List\n"),
		TempTrackerList = TrackerList -- [FromPid],
		NewTrackerList = TempTrackerList ++ [FromPid],
		NewPeers = ReceivedPeerList -- PeerList,
		NewPeerList = NewPeers ++ PeerList,
		spawn_connections(NewPeers,Record#torrent.info_sha,Id),
		loop(Record,StatusRecord,PidIndexList,NewTrackerList,NewPeerList,Id);
	    
	    {bitfield,FromPid,ReceivedBitfield} ->
		NumPieces = byte_size(Record#torrent.info#info.pieces) div 20,
		<<Bitfield:NumPieces/bitstring,_Rest/bitstring>> = ReceivedBitfield,
		PeerIndexList = bitfield:to_indexlist(Bitfield,invert),
		Intrested = is_intrested(PeerIndexList, PidIndexList),
		if 
		    Intrested ->
			FromPid ! is_interested,
			FromPid ! check_free;
		    true ->
			FromPid ! not_interested
		end,
		register_peer_process(FromPid,PeerIndexList,PidIndexList),
		loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	    {have,FromPid,Index} ->
		Intrested = is_intrested([{Index}], PidIndexList),
		if 
		    Intrested ->
			FromPid ! is_interested,
			FromPid ! check_free;
		    true ->
			FromPid ! not_interested
		end,
		register_peer_process(FromPid,[{Index}],PidIndexList),
			loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
	    {dowloaded,SenderPid,PieceIndex,Data} ->
		Done = bitfield:has_one_zero(Record#torrent.info#info.bitfield),
		case write_to_file:write(PieceIndex,Data,Record,Done) of
		    {ok, TempRecord} ->
			NewBitField = bitfield:flip_bit(PieceIndex, TempRecord#torrent.info#info.bitfield),
			NewLength = TempRecord#torrent.info#info.length_complete + byte_size(Data),
			%% 					Percentage = NewLength / TempRecord#torrent.info#info.length * 100,
			%% 					io:fwrite("....~n~.2f~n....~n", [Percentage]),
			NewRecord = TempRecord#torrent{info = (TempRecord#torrent.info)#info{bitfield = NewBitField, length_complete = NewLength}},
			torrent_db:delete_by_SHA1(NewRecord#torrent.info_sha),
			torrent_db:add(NewRecord),
			lists:keydelete(PieceIndex,1,PidIndexList),
			SenderPid ! {ok, done},
			loop(NewRecord,StatusRecord,PidIndexList,TrackerList,PeerList,Id);
		    {error, _Reason} ->
					SenderPid ! {error, corrupt_data},
			loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id)
		end;
	    {'EXIT',FromPid,_Reason} ->
		%% 			io:fwrite("~p Got EXIT: ~p\n", [FromPid, _Reason]),
		unregister_peer_process(FromPid,PidIndexList),
		loop(Record,StatusRecord,PidIndexList,TrackerList,PeerList,Id)
	end.

spawn_trackers([],_,_) ->
	ok;
spawn_trackers([Announce|AnnounceList],InfoHash,Id) ->
    Self = self(),
	spawn(tracker,init,[Self,Announce,InfoHash,Id]),
	spawn_trackers(AnnounceList,InfoHash,Id).

spawn_connections([{Ip,Port}|Rest],InfoHash,Id) ->
	spawn_link(nettransfer,init,[self(),Ip,Port,InfoHash,Id]),
	spawn_connections(Rest,InfoHash,Id);

spawn_connections([],_InfoHash,_Id) ->
	[].

recalculateConnections(PidIndexList, NetPid, Start) ->
	if
		length(PidIndexList) > Start ->
			ConnectionList = getConnections(lists:nthtail(Start, PidIndexList),[],40),
			SortedConnections = lists:keysort(3,ConnectionList),
			Result = setConnections(SortedConnections,NetPid),
			case Result of
				downloading ->
					ok;
				was_busy ->
					ok;
				not_needed ->
					recalculateConnections(PidIndexList, NetPid, Start + 1)
			end;
		true ->
			io:fwrite("haha~n"),
			NetPid ! {continue}
	end.

getConnections([],ResultList,_) ->
	ResultList;
getConnections(_,ResultList,0) ->
	ResultList;

getConnections([{_Index,Pid}|Tail],ResultList,GetCount) ->
	Pid ! {connectionsRequest,self()},
	receive
		{connection_list,ConnectionList} ->
			getConnections(Tail,[{Pid,ConnectionList,length(ConnectionList)}|ResultList],GetCount-1);
		{not_needed} ->
			getConnections(Tail,ResultList,GetCount)	
		after 50 ->
			getConnections(Tail,ResultList,GetCount)
	end.

setConnections([],_) ->
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

bind_pid_to_index([{H}|[]],_PieceLength,LastPieceSize) ->
	[{H,spawn(piece,init,[H,self(), LastPieceSize])}];

bind_pid_to_index([{H}|T],PieceLength, LastPieceSize) ->
	[{H,spawn(piece,init,[H,self(), PieceLength])}|bind_pid_to_index(T,PieceLength, LastPieceSize)].

is_intrested(PeerIndexList, PidIndexList) ->
	length([Index1 || {Index1} <- PeerIndexList, {Index2, _Pid} <- PidIndexList, Index1 == Index2]) > 0.
