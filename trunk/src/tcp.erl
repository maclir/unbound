-module(tcp).
-import(bencode, [decode/1, encode/1]).
-export([scrape/2, connect_to_server/4, open_a_socket/5, connect_to_client/4, check_handshake/2, send_a_block/4, start_listening/3, init_listening/2]).

%% THIS COMMENTED BLOCK IS FOR TESTING HERE! PLEASE DO NOT DELETE IT!

%% -----------------------------------------------------------------------------------------------------------------

% connect_to_server()-> %% this function is used to connect to our tracker and get the peer list
	% {ok,{_,_,Response}} = httpc:request(get, {"http://tiesto.barfly.se:6969/announce?info_hash=%0a%ab%5d%21%39%57%72%99%4e%64%43%cb%b3%e2%ae%03%ce%52%3b%32&peer_id=33aa6c1d95510cc140a5&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started",[]},[], []),
	% {ok,{dict,Pairs}} = decode(list_to_binary(Response)),
	% Result = lists:map(fun(X)->process_pairs(X) end, Pairs),
	% [lists:keyfind("Interval",1,Result),lists:keyfind("peers",1,Result)].

% "http://tiesto.barfly.se:6969/announce?info_hash=%0a%ab%5d%21%39%57%72%99%4e%64%43%cb%b3%e2%ae%03%ce%52%3b%32&peer_id=33aa6c1d95510cc140a5&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started"	
% http://tracker.mininova.org/announce?info_hash=%95%a2%b4%e4%51%7a%6b%55%17%7f%e6%e2%71%98%52%43%70%f2%75%22&peer_id=33aa6c1d95510cc140a5&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started	
% scrape()->
	% {ok,{_,_,Response}} = httpc:request(get, {"http://tracker.thepiratebay.org/scrape?info_hash=%8a%c3%73%1a%d4%b0%39%c0%53%93%b5%40%4a%fa%6e%73%97%81%0b%41",[]},[], []),
	% case decode(list_to_binary(Response)) of	
	% {ok,
		% {dict,
			% [{<<"files">>,
				% {dict, [{InfoHash,
							% {dict,[{<<"complete">>,Complete},
								   % {<<"downloaded">>,Downloaded},
								   % {<<"incomplete">>,Incomplete}]}}]}}]}}-> 
								   % io:format("Complete:~p~nDownloaded:~p~nIncomplete:~p~n",[Complete,Downloaded,Incomplete]);
	% _ -> io:format("Tracker does not support scraping or probably does not like you~n")
	% end.

% connect_to_client(MasterPid, Socket)-> 
	% erlang:port_connect(Socket, self()), %% since the port was opened it another process, we have to reconnect it to the current process.
	% gen_tcp:send(Socket,[  %% sending a handshake
							% 19,
							% "BitTorrent protocol",
							% <<0,0,0,0,0,0,0,0>>,
							% <<16#0a, 16#ab, 16#5d, 16#21, 16#39, 16#57, 16#72, 16#99, 16#4e, 16#64, 16#43, 16#cb, 16#b3, 16#e2, 16#ae, 16#03, 16#ce, 16#52, 16#3b, 16#32>>,
							% "BDann7c1d95510bb160a"
						% ]),
	% handshake_loop(MasterPid, <<>>), 
	% main_loop(Socket, MasterPid).
	
%% -----------------------------------------------------------------------------------------------------------------

%%
%% Tracker communiacation
%%
	
connect_to_server(AnnounceBin,InfoHashBin,ClientIdBin,Eventt)-> %% this function is used to connect to our tracker and get the peer list
    
    Announce = binary_to_list(AnnounceBin) ++ "?",
    InfoHash = "info_hash=" ++ binary_to_list(InfoHashBin) ++ "&",
    ClientId = "peer_id=" ++ binary_to_list(ClientIdBin) ++ "&",
    Port = "port=" ++ "6769" ++ "&",
    Uploaded = "uploaded=" ++ "0" ++ "&",
    Downloaded = "downloaded=" ++ "0" ++ "&",
    Left = "left=" ++ "0" ++ "&",
    Compact = "compact=" ++ "1" ++ "&",
    NoPeerId = "no_peer_id=" ++ "0" ++ "&",
	if Eventt /= "none" ->
		Event = "event=" ++ Eventt,
		RequestString = Announce ++ InfoHash ++ ClientId ++ Port ++ Uploaded ++ Downloaded ++ Left ++ Compact ++ NoPeerId ++ Event;
	true->
		RequestString = Announce ++ InfoHash ++ ClientId ++ Port ++ Uploaded ++ Downloaded ++ Left ++ Compact ++ NoPeerId
	end,
    {ok,{_,_,Response}} = httpc:request(get, {RequestString,[]},[], []),
	{ok,{dict,Pairs}} = decode(list_to_binary(Response)),
	Result = lists:map(fun(X)->process_pairs(X) end, Pairs),
	[lists:keyfind("Interval",1,Result),lists:keyfind("peers",1,Result)].
	
scrape(ScrapeBin,InfoHashBin)->	
	Scrape = binary_to_list(ScrapeBin) ++ "?",
	InfoHash = "info_hash" ++ binary_to_list(InfoHashBin),
	RequestString = Scrape ++ InfoHash,
	{ok,{_,_,Response}} = httpc:request(get, {RequestString,[]},[], []),
	case decode(list_to_binary(Response)) of	
	{ok,
		{dict,
			[{<<"files">>,
				{dict, [{InfoHash,
							{dict,[{<<"complete">>,Complete},
								   {<<"downloaded">>,Downloaded},
								   {<<"incomplete">>,Incomplete}]}}]}}]}}-> 
								   [{"Complete",Complete},{"Downloaded",Downloaded},{"Incomplete",Incomplete}];
	_ -> "Tracker does not support scraping or probably does not like you~n"
	end.

process_pairs({Key, Value})->
	case binary_to_list(Key) of
		"complete" -> {"complete", Value};
		"incomplete" -> {"incomplete",Value};
		"min interval" -> {"Min Interval",Value};
		"interval" -> {"Interval",Value};
		"peers" -> {"peers",separate(Value)};
			%%  DO NOT PANIC! ALL IO:FORMATS ARE JUST FOR TESTING. THEY ARE TO BE REPLACED WITH ACTUAL VALUE RETURNING.		
		_ -> {"Unknown pair. Key: ~p Value: ~p~n",[Key,Value]}
	end.
	
separate(<<>>)->
	[];
separate(<<Ip1:8, Ip2:8, Ip3:8, Ip4:8,Port:16,Rest/binary>>)->
	[{{Ip1,Ip2,Ip3,Ip4},Port}|separate(Rest)].
	
%%	
%% Peer Communication
%%

open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId,MasterPid)->
	case gen_tcp:connect(DestinationIp, DestinationPort, [binary, {packet,0},{active,false}]) of
		{ok,Socket} -> 
			connect_to_client(MasterPid, Socket,InfoHash,ClientId);
		{error, Reason} ->
			exit(self(), Reason)
	end.

connect_to_client(MasterPid, Socket,InfoHash,ClientId)-> 
%    erlang:port_connect(Socket, self()), %% since the port was opened it another process, we have to reconnect it to the current process.
    gen_tcp:send(Socket,[  %% sending a handshake
			   19,
			   "BitTorrent protocol",
			   <<0,0,0,0,0,0,0,0>>,
			   InfoHash,
			   ClientId
						]),
	handshake_loop(MasterPid,Socket),
	inet:setopts(Socket, [{packet, 4},{active, true}]),
	main_loop(Socket, MasterPid). %% starting the main loop for further communiation
	
handshake_loop(MasterPid, Socket)->
case gen_tcp:recv(Socket,68) of
	{ok,<< 19, "BitTorrent protocol", 
						 _ReservedBytes:8/binary, 
						 _InfoHash:20/binary, 
						 _PeerID:20/binary
						 >>}->

		MasterPid ! "peer accepted handshake";
	{error, Reason}->
			gen_tcp:close(Socket),
			exit(self(), handshake)
end.

	
main_loop(Socket, MasterPid)->
	receive
		choke ->
			gen_tcp:send(Socket,<<0>>), 
			main_loop(Socket,MasterPid); 
		unchoke ->
			gen_tcp:send(Socket,<<1>>), 
			main_loop(Socket,MasterPid); 
		keep_alive ->	%% when we send keep_alive message to the slave
			gen_tcp:send(Socket,<<0>>), %% it sends the appropriate message to the peer
			main_loop(Socket,MasterPid); %% starts the loop again
		interested ->
			gen_tcp:send(Socket,<<2>>), %% send a message, to say you are interested. for more info see bittorrent specification
			main_loop(Socket,MasterPid);
		not_interested ->
			gen_tcp:send(Socket,<<3>>),
			main_loop(Socket,MasterPid);
		{send_have, PieceIndex}->
			gen_tcp:send(Socket,[<<4:8,PieceIndex:32>>]),
			main_loop(Socket,MasterPid);
		{send_bitfield, <<Bitfield/binary>>}->
			gen_tcp:send(Socket,[<<5:8, Bitfield/binary>>]),
			main_loop(Socket,MasterPid);
		{tcp,_,<<5:8,Bitfield/binary>>} ->
			MasterPid ! {client_bitfield, self(), Bitfield},
			main_loop(Socket,MasterPid);
		{request, Index, Offset, Length} ->
			gen_tcp:send(Socket, [<<6:8, Index:32, Offset:32, Length:32>>]),
			main_loop(Socket,MasterPid);
		{send_piece,Index, Offset, Block}->
			gen_tcp:send(Socket, [<<7:8,Index:32,Offset:32,Block/binary>>]),
			main_loop(Socket,MasterPid);
		{send_cancel,Index,Offset,Length}->
			gen_tcp:send(Socket,[<<8:8,Index:32,Offset:32,Length:32>>]),
			main_loop(Socket,MasterPid);
		{send_port, Port}->
			gen_tcp:send(Socket,[<<9:8,Port:32>>]),
			main_loop(Socket,MasterPid);
		{tcp,_,<<4:8, PieceIndex:32>>} ->
			MasterPid ! {have,self(),PieceIndex},
			main_loop(Socket,MasterPid);
		{tcp,_,<<0>>} ->
			MasterPid ! {got_choked, self()},
			main_loop(Socket, MasterPid);
		{tcp,_,<<2>>}->
			MasterPid ! {got_interested,self()},
			main_loop(Socket, MasterPid);
		{tcp,_,<<5:8, Bitfield>>}->
			MasterPid ! {client_bitfield, self(), Bitfield},
			main_loop(Socket,MasterPid);
		{tcp,_,<<3>>}->
			MasterPid ! {got_not_interested, self()},
			main_loop(Socket, MasterPid);
		{tcp,_,<<>>}-> 
			MasterPid ! {got_keep_alive, self()}, %% messages, having a structre like this {tcp,_,_} show that they were recieved from the peer.
			main_loop(Socket, MasterPid); %% in this case <<>> actually means keep_alive message
		{tcp,_,<<1>>}-> 
			MasterPid ! {got_unchoked,self()}, %% process an unchoked message
			main_loop(Socket, MasterPid);
		{tcp,_,<<6:8, Index:32, Offset:32, Length:32>>}->
			send_a_block(Socket,Index, Offset, Length),
			main_loop(Socket, MasterPid);
		{tcp,_,<<8:8, Index:32, Offset:32, Length:32>>}->
			MasterPid ! {got_cancel, self(), Index, Offset, Length},
			main_loop(Socket,MasterPid);
		{tcp,_,<<9:8,Port:16>>}->
			MasterPid ! {got_port,self(),Port},
			main_loop(Socket,MasterPid);
		{tcp_closed,_}->
			gen_tcp:close(Socket),
			exit(self(), port_closed);
		{tcp,_,<<7:8, 
				_PieceIndex:32,
				Offset:32,
				Block/binary>>}->
				MasterPid ! {got_block, Offset,byte_size(Block),Block},
				main_loop(Socket, MasterPid);
		{stop,Reason} ->
			gen_tcp:close(Socket),
			exit(self(), Reason);
		{error, Reason}->
			gen_tcp:close(Socket),
			exit(self(), Reason);
		Smth ->
			MasterPid ! {"got unknown message:",Smth}, %% in case we got something really weird
			main_loop(Socket, MasterPid)
		after 10000 ->
			gen_tcp:close(Socket),
			exit(self(), main_loop_timeout)
	end.

init_listening(PortNumber,ClientId) ->
    ListeningSocket = spawn_link(tcp,start_listening,[self(),PortNumber,ClientId]),
    receive 
	{ok, _Socket} ->
	    {ok, ListeningSocket};
	{error, _ } ->
	    {error, error_opening_socket}
    end.

start_listening(InitPid, PortNumber, ClientId)->
    case gen_tcp:listen(PortNumber, [binary, {packet,0}]) of 
	 {ok, Socket} ->
	    InitPid ! {ok, Socket},
	    accepting(Socket, ClientId)
    end.
	
accepting(Socket, ClientId)->
	{ok, ListenSocket} = gen_tcp:accept(Socket),
	spawn_link(?MODULE, check_handshake,[ListenSocket,ClientId]),
	accepting(Socket,ClientId).
	
check_handshake(Socket,ClientId)->
	io:fwrite("accepting new connection ~n"),
	erlang:port_connect(Socket, self()),
	receive
		{tcp,_,<< 19, "BitTorrent protocol", 
						 ReservedBytes:8/binary, 
						 InfoHash:20/binary, 
						 PeerID:20/binary>>} ->
							MasterPid = check_infohash(Socket,InfoHash),
							send_handshake(Socket,InfoHash,ClientId),
							send_bitfield(Socket),
							main_loop(Socket, MasterPid);
		{tcp_closed,_}->
			exit(self(), "remote peer closed connection");
		{tcp,_,Msg} ->
			exit(self(), {"remote peer sent this",Msg})				
	end.
	
send_a_block(Socket, PieceIndex,Offset,Length)->
	% if smth goes wrong here, use exit(self(), "remote peer sent wrong handshake")
	ok.

send_bitfield(Socket)->
	% if smth goes wrong here, use exit(self(), "remote peer sent wrong handshake")
	ok.
	
send_handshake(Socket, InfoHash, ClientId)->
	gen_tcp:send(Socket,[
			   19,
			   "BitTorrent protocol",
			   <<0,0,0,0,0,0,0,0>>,
			   InfoHash,
			   ClientId
						]).
	
check_infohash(Socket,InfoHashFromPeer)->
	case torrent_mapper:req(InfoHashFromPeer) of
		{error,not_found} -> exit(self(), "remote peer sent wrong infohash");
		{ok,TorrentPid} ->
			{ok, IpPort} = inet:peername(Socket),
			TorrentPid ! {new_upload,self(),IpPort}
	end,
		receive
			{new_master_pid, Pid} -> 
				Pid
		end.
