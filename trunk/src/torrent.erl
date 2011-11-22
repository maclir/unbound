%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/3,init/1]).
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
	StartFunc = {torrent,start_link,[InfoHash,Id,Record]},
	ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	supervisor:start_child(Pid,ChildSpec),
	start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
	ok.



%% =============================================================================
%% Regular torrent functions

start_link(Var,Id,Record) ->
	{ok,spawn_link(torrent,init,[{Var,Id,Record}])}.

init({Var,Id,Record}) ->
    OurBitfield = Record#torrent.info#info.bitfield,
    IndexList = bitfield:to_indexlist(OurBitfield,normal),
    PieceLength = Record#torrent.info#info.piece_length,
    PidIndexList = bind_pid_to_index(IndexList,PieceLength),
    
    case Record#torrent.announce_list of
	%% If the tracker list is empty, only use the main tracker
	[] ->
	    %% Start communication with tracker and peers
	    case peerpiecemanagement:getPeerList(Record,Id) of
		[{"Interval",_Interval},{"peers",PeerList}] ->
		    peerpiecemanagement:connect_to_peer(PeerList,Record#torrent.info_sha,Id),
		    io:fwrite("~p started by client ~p\n",[Var,Id]),
		    loop(Record, #torrent_status{},PidIndexList);
		{error,Reason} ->
		    io:fwrite("~p",[Reason])
	    end;
	
	%% Should be changed so that all the trackers are queried, or should
	%% the other trackers be fallback trackers if there is no connection to
	%% the main one?
	_AnnounceList ->
	    io:fwrite("Torrent has a announce list"),
	    case peerpiecemanagement:getPeerList(Record,Id) of
		{ok,_Interval,PeerList} ->
		    peerpiecemanagement:connect_to_peer(PeerList,Record#torrent.info_sha),
		    io:fwrite("~p started by client ~p\n",[Var,Id]),
		    loop(Record, #torrent_status{},PidIndexList);
		{error,Reason} ->
		    io:fwrite("~p",[Reason])
	    end
    end.

loop(Record, StatusRecord,PidIndexList) ->
    receive
	{bitfield,FromPid,Bitfield} ->
	    PeerIndexList = bitfield:to_indexlist(Bitfield,invert),
	    piece:register_peer_process(FromPid,PeerIndexList,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList);
	{have,FromPid,Index} ->
	    piece:register_peer_process(FromPid,[{Index}],PidIndexList),
	    loop(Record,StatusRecord,PidIndexList);
	{'EXIT',FromPid,_Reason} ->
	    piece:unregister_peer_process(FromPid,PidIndexList),
	    loop(Record,StatusRecord,PidIndexList)
    end.


bind_pid_to_index([{H}|[]],PieceLength) ->
   [{H,spawn(piece,init,[H,PieceLength,true])}];

bind_pid_to_index([{H}|T],PieceLength) ->
    [{H,spawn(piece,init,[H,PieceLength,false])}|bind_pid_to_index(T,PieceLength)].






