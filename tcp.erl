-module(tcp).
-import(bencode, [decode/1, encode/1]).
-export([server/0, wait_connect/2, get_request/4, handle/1, client/1,looppy/1, send/2, connect_to_server/0, connect_to_client/2,separate2/1]).

server()->
	{ok, ListenSocket} = gen_tcp:listen(6769, [binary, {active, false}]),
	%%inet:setopts(ListenSocket, [recbuf,1000]),
	wait_connect(ListenSocket,0).
	
wait_connect(ListenSocket, Count)->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket,Count]),
	get_request1(Socket, [], Count, 0).
	
get_request1(Socket, BinaryList, Count, TotalSize) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Binary} ->
			io:fwrite("~p got",[Binary]),
			get_request(Socket, BinaryList, Count+1, TotalSize);
		{error, closed} ->
			io:fwrite("error closed~n")
	end.	
	
get_request(Socket, BinaryList, Count, TotalSize) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Binary} ->
			%%io:fwrite("~b bytes recieved ~n",[byte_size(Binary)]),
			get_request(Socket, [Binary|BinaryList], Count+1, TotalSize+byte_size(Binary));
		{error, closed} ->
			io:fwrite("got ~b bytes in ~b packages ~n",[TotalSize, Count]),
			handle(lists:reverse(BinaryList))
	end.
	
connect_to_server()->
	{ok,{_,_,V}} = httpc:request(get, {"http://tiesto.barfly.se:6969/announce?info_hash=%0a%ab%5d%21%39%57%72%99%4e%64%43%cb%b3%e2%ae%03%ce%52%3b%32&peer_id=33aa6c1d95510cc140a5&port=6769&uploaded=0&downloaded=0&left=0&compact=0&no_peer_id=0&event=started",[]},[], []),
	{ok,{dict,[{<<"interval">>,Interval},{<<"peers">>,Peers}]}} = decode(list_to_binary(V)),
	io:format("Interval: ~p~n",[Interval]),
	separate(Peers).
	
connect_to_client(Ip, Port)->
	{ok,Socket}=gen_tcp:connect(Ip, Port, [binary, {packet,4}]),
	gen_tcp:send(Socket,[
							19,
							"BitTorrent protocol",
							<<0,0,0,0,0,0,0,0>>,
							<<16#0a, 16#ab, 16#5d, 16#21, 16#39, 16#57, 16#72, 16#99, 16#4e, 16#64, 16#43, 16#cb, 16#b3, 16#e2, 16#ae, 16#03, 16#ce, 16#52, 16#3b, 16#32>>,
							"BDann7c1d95510bb160a"
						]),
	looppy(Socket).
	
looppy(Socket)->
	receive
		{tcp,_,<< 19, "BitTorrent protocol", 
						_ReservedBytes:8/binary, 
						_InfoHash:20/binary, 
						_PeerID:20/binary >> } ->
			io:format("peer accepted handshake~n");
		M ->
			io:format("~p~n", [M])
		after 10000 ->
			gen_tcp:send(Socket, <<>>),
			io:format("keepAlive sent"),
			looppy(Socket)
	end.
	
separate2(<<>>)->
	ok;
separate2(<<Smth:8, Rest/binary>>)->
	io:format("~p, ", [Smth]),
	separate2(Rest).

separate(<<>>)->
	ok;
separate(<<Ip1:8, Ip2:8, Ip3:8, Ip4:8,Port:16,Rest/binary>>)->
	io:format("~p.~p.~p.~p:~p ~n",[Ip1,Ip2,Ip3,Ip4,Port]),
	separate(Rest).
	
handle(Binary) ->
	{ok, Fd} = file:open("D:\\how_do_i_send_a_filename", write),
	file:write(Fd,Binary),
	file:close(Fd).
	 
	


		
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
