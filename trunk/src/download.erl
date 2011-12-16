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
		{die} ->
			kill_pieces(Downloading),
			exit(self(), stopped);
		{new_free, NetPid} ->
			case allocate_net(Downloading, NetPid) of
				done ->
					{NewQue, NewDownloading} = {Que, Downloading};
				spawn_more ->
					{NewQue, NewDownloading} = spawn_piece(Que, Downloading, NetPid, Que, TorrentPid)
			end,
			loop(NewQue,NewDownloading, TorrentPid);
		{net_index_list, NetPid, IndexList} ->
			{QueStat, NewQue} = alter_list(IndexList, Que, NetPid, false, []),
			{DownloadingStat, NewDownloading} = alter_list(IndexList, Downloading, NetPid, false, []),
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

alter_list(_, [], _NetPid, Status, NewList) ->
	{Status, sort(NewList)};
alter_list(IndexList, [{PieceIndex, NetPidList, PieceInfo}|List], NetPid, Status, TempList) ->
	NewStatus = lists:keymember(PieceIndex, 1 , IndexList),
	case NewStatus of
		false ->
			NewList = [{PieceIndex, NetPidList, PieceInfo}|TempList];
		_ ->
			NewList = [{PieceIndex, [NetPid | NetPidList], PieceInfo}|TempList]
	end,
	alter_list(IndexList, List, NetPid, NewStatus or Status, NewList).

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
kill_pieces([]) ->
	ok;
kill_pieces([{_PieceIndex, _NetPidList, PiecePid}|_Downloading]) ->
	exit(PiecePid, stopped).

sort(List) ->
%% 	List.
	SortFun = fun(X, Y) -> length(element(2, X)) < length(element(2, Y)) end,
	lists:sort(SortFun, List).