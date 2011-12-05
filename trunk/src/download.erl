-module(download).

-export([init/2, sort/1]).
-include("torrent_db_records.hrl").

init(Record, TorrentPid)->
	PieceLength = Record#torrent.info#info.piece_length,
	TotalLength = Record#torrent.info#info.length,
	TempLastPieceLength = TotalLength rem PieceLength,

	case TempLastPieceLength of
		0 ->
			LastPieceLength = PieceLength;
		_ ->
			LastPieceLength = TempLastPieceLength
	end,

	OurBitfield = <<(Record#torrent.info#info.bitfield)/bitstring>>,
	Que = bitfield:to_indexlist_que(OurBitfield, PieceLength, LastPieceLength),
	
	Downloading = [],

	loop(Que, Downloading, TorrentPid).

loop(Que, Downloading, TorrentPid) ->
	receive
		{new_free, NetPid} ->
			case allocate_net(Downloading, NetPid) of
				done ->
					{NewQue, NewDownloading} = {Que, Downloading};
				spawn_more ->
					{NewQue, NewDownloading} = spawn_piece(Que, Downloading, NetPid, Que, TorrentPid)
			end,
			loop(NewQue,NewDownloading, TorrentPid);
		{net_index_list, NetPid, IndexList} ->
			{QueStat, NewQue} = alter_list(IndexList, Que, NetPid, false),
			{DownloadingStat, NewDownloading} = alter_list(IndexList, Downloading, NetPid, false),
			if
				(QueStat or DownloadingStat) ->
					NetPid ! is_interested,
					NetPid ! check_free;
				true ->
					NetPid ! not_interested
			end,
			loop(NewQue, NewDownloading, TorrentPid);
		{piece_done, PieceIndex} ->
			NewDownloading = lists:keydelete(PieceIndex, 1, Downloading),
			loop(Que, NewDownloading, TorrentPid);
		{net_exited, NetPid} ->
			NewQue = remove_from_list(Que, NetPid, []),
			NewDownloading = remove_from_list(Downloading, NetPid, []),
			loop(NewQue, NewDownloading, TorrentPid)
	end.

remove_from_list([], _NetPid, List)->
	sort(List);
remove_from_list([{PieceIndex, NetPidList, PieceInfo}|T], NetPid, List)->
	case lists:member(NetPid, NetPidList) and is_pid(PieceInfo) of
		true ->
			PieceInfo ! {unregister, NetPid};
		_ ->
			ok
	end,
	remove_from_list(T, NetPid, [{PieceIndex, lists:delete(NetPid, NetPidList), PieceInfo}|List]).

alter_list([], List, _NetPid, Status) ->
	{Status, sort(List)};
alter_list([{Index}|IndexList], List, NetPid, Status) ->
	{NewStatus, NewList} = add_to_list(Index, List, NetPid,[], Status),
	alter_list(IndexList, NewList, NetPid, NewStatus).

add_to_list(_Index, [], _NetPid, List, Status) ->
	{Status, List};
add_to_list(PieceIndex, [{PieceIndex, NetPidList, PieceInfo}|T], NetPid, List, _Status) ->
	add_to_list(PieceIndex, T, NetPid,[{PieceIndex, [NetPid|NetPidList], PieceInfo}|List], true);
add_to_list(Index, [H|T], NetPid, List, Status) ->
	add_to_list(Index, T, NetPid, [H|List], Status).
	

allocate_net([], _NetPid) ->
	spawn_more;
allocate_net([{_PieceIndex, NetPidList, PiecePid}|Downloading], NetPid) ->
	case lists:member(NetPid, NetPidList) and is_process_alive(PiecePid) of
		true ->
			PiecePid ! {new_net_pid,self(),NetPid},
			receive
				needed ->
					done;
				not_needed ->
					allocate_net(Downloading, NetPid)
			after 500 ->
					allocate_net(Downloading, NetPid)
			end;
		false ->
			allocate_net(Downloading, NetPid)
	end.

spawn_piece([], Downloading, NetPid, RawQue, _TorrentPid) ->
	NetPid ! not_interested,
	{RawQue, Downloading};
spawn_piece([{PieceIndex, NetPidList, PieceLength}|Que], Downloading, NetPid, RawQue, TorrentPid) ->
	case lists:member(NetPid, NetPidList) of
		true ->
			PiecePid = spawn(piece,init,[PieceIndex, TorrentPid, PieceLength]),
			NewQue = lists:keydelete(PieceIndex, 1, RawQue),
			NewDownloading = [{PieceIndex, NetPidList, PiecePid}|Downloading],
			case allocate_net([{PieceIndex, NetPidList, PiecePid}], NetPid) of
				done ->
					{NewQue, NewDownloading};
				spawn_more ->
					spawn_piece(NewQue, NewDownloading, NetPid, RawQue, TorrentPid)
			end;
		false ->
			spawn_piece(Que, Downloading, NetPid, RawQue, TorrentPid)
	end.

sort(List) ->
%% 	List.
	SortFun = fun(X, Y) -> length(element(2, X)) < length(element(2, Y)) end,
	lists:sort(SortFun, List).