-module(tcp).
-import(bencode, [decode/1, encode/1]).
-export([scrape/0, server/0, wait_connect/2, get_request/4, handle/1, client/1, send/2, connect_to_server/4, open_a_socket/2, connect_to_client/2]).

%%
%% Tracker communiacation
%%

%% THIS COMMENTED BLOCK IS FOR TESTING HERE! DO NOT DELETE IT!
%% -----------------------------------------------------------
% connect_to_server()-> %% this function is used to connect to our tracker and get the peer list
	% {ok,{_,_,Response}} = httpc:request(get, {"http://tiesto.barfly.se:6969/announce?info_hash=%0a%ab%5d%21%39%57%72%99%4e%64%43%cb%b3%e2%ae%03%ce%52%3b%32&peer_id=33aa6c1d95510cc140a5&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started",[]},[], []),
	% {ok,
		% {dict,
			% [{<<"interval">>,Interval},
				% {<<"peers">>,Peers}
			% ]
		% }
	% } = decode(list_to_binary(Response)), %% this separates peer list from everything else
	% io:format("Interval: ~p~n",[Interval]), %% prints the interval
	% separate(Peers). %% formating a peer list

% scrape()->
	% {ok,{_,_,Response}} = httpc:request(get, {"http://tiesto.barfly.se:6969/scrape?info_hash=%0a%ab%5d%21%39%57%72%99%4e%64%43%cb%b3%e2%ae%03%ce%52%3b%32",[]},[], []),
	% {ok,
		% {dict,
			% [{<<"files">>,
				% {dict, [{InfoHash,
							% {dict,[{<<"complete">>,Complete},
								   % {<<"downloaded">>,Downloaded},
								   % {<<"incomplete">>,Incomplete}]}}]}}]}} = decode(list_to_binary(Response)).
%% -----------------------------------------------------------
	
scrape(ScrapeBin,InfoHashBin)->	
	Scrape = bianry_to_list(AnnounceBin) ++ "?",
	InfoHash = "info_hash" ++ binary_to_list(InfoHashBin),
	RequestString = Scrape ++ InfoHash,
	
	{ok,{_,_,Response}} = httpc:request(get, {RequestString,[]},[], []),
	{ok,
		{dict,
			[{<<"files">>,
				{dict, [{InfoHash,
							{dict,[{<<"complete">>,Complete},
								   {<<"downloaded">>,Downloaded},
								   {<<"incomplete">>,Incomplete}]
							}}]}}]}} = decode(list_to_binary(Response)).
	
	
connect_to_server(AnnounceBin,InfoHashBin,ClientIdBin,Event)-> %% this function is used to connect to our tracker and get the peer list
    
    %% Code for building the request string that is sent to the tracker
    Announce = binary_to_list(AnnounceBin) ++ "?",
    InfoHash = "info_hash=" ++ binary_to_list(InfoHashBin) ++ "&",
    ClientId = "peer_id=" ++ binary_to_list(ClientIdBin) ++ "&",
    Port = "port=" ++ "6769" ++ "&",
    Uploaded = "uploaded=" ++ "0" ++ "&",
    Downloaded = "downloaded=" ++ "0" ++ "&",
    Left = "left=" ++ "0" ++ "&",
    Compact = "compact=" ++ "0" ++ "&",
    NoPeerId = "no_peer_id=" ++ "0" ++ "&",
    Event = "event=" ++ Event,
    RequestString = Announce ++ InfoHash ++ ClientId ++ Port ++ Uploaded ++ Downloaded ++ Left ++ Compact ++ NoPeerId ++ Event,

    %% Code for making the request and parsing the trackers response
    {ok,{_,_,Response}} = httpc:request(get, {RequestString,[]},[], []),
    {ok,{dict, [{<<"interval">>,Interval}, {<<"peers">>,Peers}]}} = decode(list_to_binary(Response)), %% this separates peer list from everything else
    PeerList = separate(Peers), %% formating a peer list
    {ok,Interval,PeerList}.


	
separate(<<>>)->
	ok;
separate(<<Ip1:8, Ip2:8, Ip3:8, Ip4:8,Port:16,Rest/binary>>)->
	io:format("~p.~p.~p.~p:~p ~n",[Ip1,Ip2,Ip3,Ip4,Port]),
	separate(Rest).
	
%%	
%% Peer Communication
%%

open_a_socket(DestinationIp, DestinationPort)->
	{ok,Socket}=gen_tcp:connect(DestinationIp, DestinationPort, [binary, {packet,0}]),
	case whereis(slave) of %% we make a separate process for communiacation with a peer
		undefined ->
			register(slave, spawn(?MODULE, connect_to_client,[self(), Socket]))
	end,
	Socket.

connect_to_client(MasterPid, Socket)-> 
	erlang:port_connect(Socket, self()), %% since the port was opened it another process, we have to reconnect it to the current process.
	gen_tcp:send(Socket,[  %% sending a handshake
							19,
							"BitTorrent protocol",
							<<0,0,0,0,0,0,0,0>>,
							<<16#0a, 16#ab, 16#5d, 16#21, 16#39, 16#57, 16#72, 16#99, 16#4e, 16#64, 16#43, 16#cb, 16#b3, 16#e2, 16#ae, 16#03, 16#ce, 16#52, 16#3b, 16#32>>,
							"BDann7c1d95510bb160a"
						]),
	handshake_loop(MasterPid, <<>>), %% starting a loop for handling handshaking
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
		process_bitfield(MasterPid, Rest).
process_bitfield(MasterPid, <<_BitFieldLengthPrefix:4/binary, Rest/binary>>)->
		 BitFieldLengthPrefix = lists:nth(length(binary_to_list(_BitFieldLengthPrefix)),binary_to_list(_BitFieldLengthPrefix)),
		 process_bitfield_payload(MasterPid, BitFieldLengthPrefix, Rest).
process_bitfield_payload(MasterPid, BitFieldLengthPrefix, Rest)->
		<<BitField:BitFieldLengthPrefix/binary,Rest1/binary>> = Rest,
		if byte_size(Rest1) >= 9 ->
			process_have_messages(MasterPid, Rest1,[]);
		true ->
			self() ! {tcp,thisIsADummyVariableAnyway,Rest1}
		end.
process_have_messages(MasterPid, <<>>, Result)->
		MasterPid ! Result;
process_have_messages(MasterPid, <<HaveMessageLengthPrefix:5/binary, HaveMessagePayload:4/binary, Rest/binary>>, Result)->
		<<PieceIndex:32>> = HaveMessagePayload,
		process_have_messages(MasterPid, Rest, [{tcp,have,PieceIndex}|Result]).
	
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
		{piece, Index, Offset, Length} ->
			gen_tcp:send(Socket, [<<13:32,6:8, Index:32, Offset:32, Length:32>>]),
			HoleBlock = process_block(MasterPid, Length, <<>>),
			MasterPid ! {"got the block:", HoleBlock}, 
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
