%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(request_handler).
-export([start_link/1]).

start_link(TcpPid) ->
    loop(TcpPid).

loop(TcpPid) ->
    receive
	{handshake,TorrentId,ClientId} ->
	    case torrent:mapper(TorrentId) of
		{ok,TorrentPid} ->
		    Tcp ! send_handshake,
		    TorrentPid ! return_bitfield;
		{error,_} ->
		    %% Close tcp connection and exit
		    io:fwrite("TCP Connection closed")
	    end;
	{ok,Bitfield} ->
	    TcpPid ! send_bitfield;
	{interested} ->
	    TcpPid ! unchoke,
	    io:frwrite("Peer is interested\n");
	{not_interested} ->
	    io:fwrite("Got not interested from peer\n");
	{request,Piece,Offset} ->
	    io:fwrite("Got piece request\n")
    end.

