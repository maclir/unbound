%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(tracker).
-export([init/1]).

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
	{_,_} ->
	    ok
    after Interval ->
	    UrlInfoHash = info_hash:url_encode(InfoHash),
	    perform_request(TorrentPid,Announce,UrlInfoHash,Id,"started").
    end.

loop(TorrentPid,Announce,UrlInfoHash,Id,Interval) ->
    receive
	{stopped} ->
	    perform_request(TorrentPid,Announce,UrlInfoHash,Id,"stopped");
	{completed} ->
	    perform_request(TorrentPid,Announce,UrlInfoHash,Id,"completed")
    after Interval ->
	    perform_request(TorrentPid,Announce,UrlInfoHash,Id,undefined)

send_request(Announce,UrlInfoHash,Id,Event) ->    
    case tcp:connect_to_server(Announce,UrlInfoHash,Id,Event) of
	[{"Interval",_Interval},{"peers",PeerList}] ->
	    TorrentPid ! {ok,PeerList},
	    loop(TorrentPid,Announce,UrlInfoHash,Id,Interval);
	{error,Reason} ->
	    TorrentPid ! {error,Reason}
    end;
	

