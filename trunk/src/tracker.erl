%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(tracker).
-export([init/4]).

%%----------------------------------------------------------------------
%% Function:   init/4
%% Purpose:    evaluates the given url and if it is http url connects to the tracker
%% Args:       TorrentPid(pid),Announce(string),InfoHash(string),Id(string)
%%----------------------------------------------------------------------
init(TorrentPid,Announce,InfoHash,Id) ->
	case Announce of
		<<"http",_Rest/binary>> ->
			UrlInfoHash = info_hash:url_encode(InfoHash),
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"started",200, tcp);
		<<"udp",_Rest/binary>> ->
			perform_request(TorrentPid,Announce,InfoHash,Id,"started",200, {udp, connection_id});
		_Var ->
			ok
	end.

%%----------------------------------------------------------------------
%% Function:  loop/6
%% Purpose:   waits for commands coming from torrent if no commands is received
%%            within the interval given by the tracker, it sends a request to
%%            torrent and also if a command is received, then it sends it to the tracker.
%% Args:      TorrentPid(pid),Announce(string),UrlInfoHash(string),Id(string),Interval(integer)
%%----------------------------------------------------------------------
loop(TorrentPid,Announce,UrlInfoHash,Id,Interval,Type) ->
	receive
		{stopped} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"stopped",1, Type),
			exit(self(),stopped);
		{completed} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"completed",1, Type);
		{get_peers} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"none",100, Type)
		after Interval * 1000 ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"none",50, Type)
	end.

%%----------------------------------------------------------------------
%% Function:  perform_request/7
%% Purpose:   performs the requests
%% Args:      TorrentPid(pid),Announce(string),UrlInfoHash(string),Id(string),Event(string),NumWanted(integer)
%% Returns:   peerlist retrieved from the tracker.
%%----------------------------------------------------------------------
perform_request(TorrentPid,Announce,UrlInfoHash,Id,Event,NumWanted, Type) ->
	Self = self(),
	TorrentPid ! {get_statistics,Self},
	receive
		{statistics, Uploaded, Downloaded, Left} ->
			case tcp:connect_to_server(Announce,UrlInfoHash,Id,Event,Uploaded,Downloaded,Left,NumWanted, Type) of
				[{"Interval",Interval},{"peers",PeerList}] ->
					TorrentPid ! {peer_list,self(),PeerList},
					loop(TorrentPid,Announce,UrlInfoHash,Id,Interval, Type);
				{connection_id,ConnectionId} ->
					[{"Interval",Interval},{"peers",PeerList}] = tcp:connect_to_server(Announce,UrlInfoHash,Id,Event,Uploaded,Downloaded,Left,NumWanted, {udp, ConnectionId}),
					TorrentPid ! {peer_list,self(),PeerList},
					loop(TorrentPid,Announce,UrlInfoHash,Id,Interval, {udp, ConnectionId});
				{error,Reason} ->
					TorrentPid ! {error,Reason};
				_K ->
					ok
			end
	end.


