-module(tcp).
-import(bencode, [decode/1, encode/1]).
-export([server/0, wait_connect/2, get_request/4, handle/1, client/1, send/2, connect_to_server/3, open_a_socket/2, connect_to_client/2]).

%%
%% Tracker communiacation
%%

connect_to_server(AnnounceBin,InfoHashBin,ClientIdBin)-> %% this function is used to connect to our tracker and get the peer list
    Announce = binary_to_list(AnnounceBin),
    InfoHash = binary_to_list(InfoHashBin),
    ClientId = binary_to_list(ClientIdBin),
    {ok,{_,_,Response}} = httpc:request(get, {Announce ++ "?info_hash=" ++ InfoHash ++ "&peer_id=" ++ ClientId ++ "&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started",[]},[], []),
	{ok,
		{dict,
			[{<<"interval">>,Interval},
				{<<"peers">>,Peers}
			]
		}
	} = decode(list_to_binary(Response)), %% this separates peer list from everything else
	io:format("Interval: ~p~n",[Interval]), %% prints the interval
	separate(Peers). %% formating a peer list
	
separate(<<>>)->
	ok;
separate(<<Ip1:8, Ip2:8, Ip3:8, Ip4:8,Port:16,Rest/binary>>)->
	io:format("~p.~p.~p.~p:~p ~n",[Ip1,Ip2,Ip3,Ip4,Port]),
	separate(Rest).
	
%%	
%% Peer Communication
%%

open_a_socket(DestinationIp, DestinationPort)->
	{ok,Socket}=gen_tcp:connect(DestinationIp, DestinationPort, [binary, {packet,0}]), %% pay attention to {packet,0}.
																					%% when this parameter is set to 0, no packaging is done.
																					%% check here for more info:
																					%% http://www.erlang.org/doc/man/inet.html#setopts-2
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
	handshake_loop(Socket, MasterPid), %% starting a loop for handling handshaking
	inet:setopts(Socket, [{packet, 4}]), %% if you visited the link above, this should be more or less clear
	main_loop(Socket, MasterPid). %% starting the main loop for further communiation
	
handshake_loop(Socket, MasterPid)->
	receive
		{tcp,_,<< 19, "BitTorrent protocol",  %% pattern match the handshake response
						_ReservedBytes:8/binary, 
						_InfoHash:20/binary, 
						_PeerID:20/binary >> } ->
			MasterPid ! "peer accepted handshake", %% if correct, send an appropriate message to the parent
			handshake_loop(Socket, MasterPid); %% start the loop again, because there is also a bitfield coming.
		M ->
			MasterPid ! {"got this during handshake: ", M}, %% catching bitfield
			handshake_loop(Socket, MasterPid) %% starting the loop again, cuz there might some messages coming as well (depends on a peer client)
		after 5000 ->
			ok %% stop the loop after 5 seconds
	end.

%% this loop processes ALL messages. The ones it gets from the peer AND the ones we send to it, from the parent process
%% to send a message from the parent process, as a common structure: <process name> ! <message body>, for example: slave ! keep_alive.	
main_loop(Socket, MasterPid)->
	receive
		keep_alive ->	%% when we send keep_alive message to the slave
			gen_tcp:send(Socket,<<>>), %% it sends the appropriate message to the peer
			main_loop(Socket,MasterPid); %% starts the loop again
		interested ->
			gen_tcp:send(Socket,<<2>>), %% send a message, to say you are interested. for more info see bittorrent specification
			main_loop(Socket,MasterPid);
		{piece, Index, Offset, Length} ->
			gen_tcp:send(Socket, [<<6:8, Index:32, Offset:32, Length:32>>]), %% this is the request syntax. this is how we request a piece
			main_loop(Socket,MasterPid);
		{tcp,_,<<>>}-> 
			MasterPid ! got_keep_alive, %% messages, having a structre like this {tcp,_,_} show that they were recieved from the peer.
			main_loop(Socket, MasterPid); %% in this case <<>> actually means keep_alive message
		{tcp,_,<<1>>}-> 
			MasterPid ! got_unchoked, %% process an unchoked message
			main_loop(Socket, MasterPid);
		{tcp,_,<<_LengthPrefix:7/binary, %% pattern matching the response, when we requested a piece
				_PieceIndex:1/binary,	%% variables names speak for themselves
				_Offset:1/binary,		%% for more info see the specification
				Block/binary>>}->
			MasterPid ! {"got the block: ", Block}, 
			main_loop(Socket, MasterPid);
		{tcp,_,M}->   
			MasterPid ! {"got this during main loop:", M}, %% this is for the case when we got some other unrecognized message from a peer
			main_loop(Socket, MasterPid);
		stop ->
			MasterPid ! stopping; %% this is made to stop the slave process. The loop is not being called again here.
		Smth ->
			MasterPid ! {"got unknown message:",Smth}, %% in case we got something really weird
			main_loop(Socket, MasterPid)
	end. %% there is no recieving loop for the parent process, so, to actually see, what the slave sends to you, use the flush() function.
	
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
