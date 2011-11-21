-module(tcp).
-import(bencode, [decode/1, encode/1]).
-export([scrape/2, server/0, wait_connect/2, get_request/4, handle/1, client/1, send/2, connect_to_server/4, open_a_socket/4, connect_to_client/4]).

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
    Compact = "compact=" ++ "0" ++ "&",
    NoPeerId = "no_peer_id=" ++ "0" ++ "&",
    Event = "event=" ++ Eventt,
    RequestString = Announce ++ InfoHash ++ ClientId ++ Port ++ Uploaded ++ Downloaded ++ Left ++ Compact ++ NoPeerId ++ Event,

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

open_a_socket(DestinationIp, DestinationPort, InfoHash, ClientId)->
	{ok,Socket}=gen_tcp:connect(DestinationIp, DestinationPort, [binary, {packet,0}]),
	spawn(?MODULE, connect_to_client,[self(), Socket, InfoHash,ClientId]).

connect_to_client(MasterPid, Socket,InfoHash,ClientId)-> 
    erlang:port_connect(Socket, self()), %% since the port was opened it another process, we have to reconnect it to the current process.
    gen_tcp:send(Socket,[  %% sending a handshake
			   19,
			   "BitTorrent protocol",
			   <<0,0,0,0,0,0,0,0>>,
			   InfoHash,
			   ClientId
						]),
	handshake_loop(MasterPid,<<>>), %% starting a loop for handling handshaking
	main_loop(Socket, MasterPid). %% starting the main loop for further communiation
	
handshake_loop(MasterPid,HandshakeResponse)->
	receive
		{tcp,_,Msg} ->
			handshake_loop(MasterPid,<<HandshakeResponse/binary,Msg/binary>>)
		after 5000 ->
			process_handshake(MasterPid, HandshakeResponse)%% stop the loop after 5 seconds
			
	end.
process_handshake(MasterPid, <<>>)->
	MasterPid ! "some wierd shit happened";
process_handshake(MasterPid, << 19, "BitTorrent protocol", 
						 _ReservedBytes:8/binary, 
						 _InfoHash:20/binary, 
						 _PeerID:20/binary,
						 Rest/binary >>)->
		MasterPid ! "peer accepted handshake~n",
		process_bitfield(Rest).
process_bitfield(<<_BitFieldLengthPrefix:4/binary, Rest/binary>>)->
		 BitFieldLengthPrefix = lists:nth(length(binary_to_list(_BitFieldLengthPrefix)),binary_to_list(_BitFieldLengthPrefix)),
		 process_bitfield_payload(BitFieldLengthPrefix, Rest).
process_bitfield_payload(BitFieldLengthPrefix, Rest)->
		<<BitField:BitFieldLengthPrefix/binary,Rest1/binary>> = Rest,
		if byte_size(Rest1) >= 9 ->
			process_have_messages(Rest1);
		true ->
			self() ! {bitfield,BitField}
		end.
process_have_messages(<<>>)->
		ok;
process_have_messages(<<HaveMessage:9/binary, Rest/binary>>)->
		self() ! {have, self(), HaveMessage},
		process_have_messages(Rest).
	
%% this loop processes ALL messages. The ones it gets from the peer AND the ones we send to it, from the parent process
%% to send a message from the parent process, as a common structure: <process name> ! <message body>, for example: slave ! keep_alive.	
main_loop(Socket, MasterPid)->
	receive
		choke ->
			gen_tcp:send(Socket,<<0,0,0,1,0>>), 
			main_loop(Socket,MasterPid); 
		unchoke ->
			gen_tcp:send(Socket,<<0,0,0,1,1>>), 
			main_loop(Socket,MasterPid); 
		keep_alive ->	%% when we send keep_alive message to the slave
			gen_tcp:send(Socket,<<0,0,0,0>>), %% it sends the appropriate message to the peer
			main_loop(Socket,MasterPid); %% starts the loop again
		interested ->
			gen_tcp:send(Socket,<<1:32,2:8>>), %% send a message, to say you are interested. for more info see bittorrent specification
			main_loop(Socket,MasterPid);
		not_interested ->
			gen_tcp:send(Socket,<<0,0,0,1,3>>),
			main_loop(MasterPid, Socket);
		{bitfield,Rest1} ->
			MasterPid ! {client_bitfield, self(), Rest1},
			main_loop(Socket,MasterPid);
		{piece, Index, Offset, Length} ->
			gen_tcp:send(Socket, [<<13:32,6:8, Index:32, Offset:32, Length:32>>]),
			HoleBlock = process_block(MasterPid, Length, <<>>),
			MasterPid ! {"got the block:", HoleBlock}, 
			main_loop(Socket,MasterPid);
		{have,From,<<5:32, 4:8, PieceIndex:32>>} ->
			MasterPid ! {have,self(),PieceIndex},
			main_loop(Socket,MasterPid);
		{tcp,_,<<0,0,0,0>>}-> 
			MasterPid ! got_keep_alive, %% messages, having a structre like this {tcp,_,_} show that they were recieved from the peer.
			main_loop(Socket, MasterPid); %% in this case <<>> actually means keep_alive message
		{tcp,_,<<1:32,1:8>>}-> 
			MasterPid ! got_unchoked, %% process an unchoked message
			main_loop(Socket, MasterPid);
		stop ->
			MasterPid ! stopping; %% this is made to stop the slave process. The loop is not being called again here.
		Smth ->
			MasterPid ! {"got unknown message:",Smth}, %% in case we got something really weird
			main_loop(Socket, MasterPid)
	end.
	
process_block(MasterPid, Length, Result)->
	if byte_size(Result)==0 ->
		receive
		{tcp,_,<<_LengthPrefix:32,
				7:8, %% pattern matching the response, when we requested a piece
				_PieceIndex:32,	%% variables names speak for themselves
				_Offset:32,		%% for more info see the specification
				PieceOfTheBlock/binary>>}->
				if Length == byte_size(<<Result/binary, PieceOfTheBlock/binary>>)->
					<<Result/binary, PieceOfTheBlock/binary>>;
				true->
				process_block(MasterPid, Length, <<Result/binary,PieceOfTheBlock/binary>>)
				end
		end;
	true ->
	receive
		{tcp,_,<<PieceOfTheBlock/binary>>}->
				if Length == byte_size(<<Result/binary, PieceOfTheBlock/binary>>)->
					<<Result/binary, PieceOfTheBlock/binary>>;
				true->
				process_block(MasterPid, Length, <<Result/binary,PieceOfTheBlock/binary>>)
				end
		end
	end.

%%
%% File transferring from one computer to another
%%	

server()->
	{ok, ListenSocket} = gen_tcp:listen(6769, [binary, {active, false}]),
	%%inet:setopts(ListenSocket, [recbuf,1000]),
	wait_connect(ListenSocket,0).
	
wait_connect(ListenSocket, Count)->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket,Count]),
	get_request(Socket, [], Count, 0).
	
get_request(Socket, BinaryList, Count, TotalSize) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Binary} ->
			%%io:fwrite("~b bytes recieved ~n",[byte_size(Binary)]),
			get_request(Socket, [Binary|BinaryList], Count+1, TotalSize+byte_size(Binary));
		{error, closed} ->
			io:fwrite("got ~b bytes in ~b packages ~n",[TotalSize, Count]),
			handle(lists:reverse(BinaryList))
	end.	
	
handle(Binary) ->
	{ok, Fd} = file:open("D:\\how_do_i_send_a_filename", write),
	file:write(Fd,Binary),
	file:close(Fd).
	
%% Here starts the client part.
	
client(Host) ->
	{ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
	{ok, File} = file:read_file("a.jpg"),
	send(Socket, File).
	
send(Socket, <<Chunk:1000/binary, Rest/binary>>) ->	
	%%io:fwrite("~b~n",[byte_size(Chunk)]),
	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);
send(Socket, Rest) ->
io:fwrite("~b~n",[byte_size(Rest)]),
	gen_tcp:send(Socket, Rest),
	ok = gen_tcp:close(Socket).
