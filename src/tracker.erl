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
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"started",200);
		<<"udp",_Rest/binary>> ->
			TorrentPid ! {error,udp_not_supported};
		_Var ->
			ok
	end.

%%----------------------------------------------------------------------
%% Function:  loop/5
%% Purpose:   waits for commands coming from torrent if no commands is received
%%            within the interval given by the tracker, it sends a request to
%%            torrent and also if a command is received, then it sends it to the tracker.
%% Args:      TorrentPid(pid),Announce(string),UrlInfoHash(string),Id(string),Interval(integer)
%%----------------------------------------------------------------------
loop(TorrentPid,Announce,UrlInfoHash,Id,Interval) ->
	receive
		{stopped} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"stopped",0),
			exit(self(),stopped);
		{completed} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"completed",0);
		{get_peers} ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"none",100)
		after Interval*10 ->
			perform_request(TorrentPid,Announce,UrlInfoHash,Id,"none",50)
	end.

%%----------------------------------------------------------------------
%% Function:  perform_request/6
%% Purpose:   performs the requests
%% Args:      TorrentPid(pid),Announce(string),UrlInfoHash(string),Id(string),Event(string),NumWanted(integer)
%% Returns:   peerlist retrieved from the tracker.
%%----------------------------------------------------------------------
perform_request(TorrentPid,Announce,UrlInfoHash,Id,Event,NumWanted) ->
	Self = self(),
	TorrentPid ! {get_statistics,Self},
	receive
		{statistics, Uploaded, Downloaded, Left} ->
			case tcp:connect_to_server(Announce,UrlInfoHash,Id,Event,Uploaded,Downloaded,Left,NumWanted) of
				[{"Interval",Interval},{"peers",PeerList}] ->
					TorrentPid ! {peer_list,self(),PeerList},
					loop(TorrentPid,Announce,UrlInfoHash,Id,Interval);
				{error,Reason} ->
					TorrentPid ! {error,Reason};
				_K ->
					ok
			end
	end.


