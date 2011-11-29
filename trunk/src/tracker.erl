%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(tracker).
-export([init/4]).

init(TorrentPid,Announce,InfoHash,Id) ->
	case Announce of
		<<"http",_Rest/binary>> ->
			UrlInfoHash = info_hash:url_encode(InfoHash),
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"started");
		<<"udp",_Rest/binary>> ->
			TorrentPid ! {error,udp_not_supported}
	end.


loop(TorrentPid,Announce,UrlInfoHash,Id,Interval) ->
	receive
		{stopped} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"stopped");
		{completed} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"completed")
		after Interval*1000 ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"started")
	end.

perform_request(TorrentPid,Announce,UrlInfoHash,Id,Event) ->
	case tcp:connect_to_server(Announce,UrlInfoHash,Id,Event) of
		[{"Interval",Interval},{"peers",PeerList}] ->
			TorrentPid ! {peer_list,self(),PeerList},
			loop(TorrentPid,Announce,UrlInfoHash,Id,Interval);
		{error,Reason} ->
			io:fwrite("Tracker.erl error ~p~n", [Reason]),
			TorrentPid ! {error,Reason};
		K ->
			io:fwrite("Tracker.erl error ~p~n", [K])
	end.


