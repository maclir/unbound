%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		main module for the webserver
%%%----------------------------------------------------------------------
-module(web_tcp).
-export([start/0, start/1, stop/0, stop/1]).

	       
%%----------------------------------------------------------------------
%% Function:	start/0
%% Purpose:		same as start/1 with default port of 9999
%% Returns:		{ok, Port (integer)} (successfully started) or 
%%				{already_started, Port (integer)} already started on
%%				this port
%%----------------------------------------------------------------------
start() ->
    start(9999).

%%----------------------------------------------------------------------
%% Function:	start/1
%% Purpose:		start listening on Port
%% Args:		Port (integer)
%% Returns:		{ok, Port (integer)} (successfully started) or 
%%				{already_started, Port (integer)} already started on
%%				this port
%%----------------------------------------------------------------------
start(Port) ->
	Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Pid = spawn(fun () -> start_listen(Port) end),
	    register(Name, Pid);
	_ ->
	    {already_started, Port}
    end.

%%----------------------------------------------------------------------
%% Function:	stop/0
%% Purpose:		same as stop/1 with default port of 9999
%%				stopped (successfully stopped)
%%----------------------------------------------------------------------
stop() ->
	stop(9999).

%%----------------------------------------------------------------------
%% Function:	stop/1
%% Purpose:		stop listening on Port
%% Args:		Port (integer)
%% Returns:		not_started (server is not listening on Port) or
%%				stopped (successfully stopped)
%%----------------------------------------------------------------------
stop(Port) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    not_started;
	Pid ->
	    exit(Pid, kill),
	    (catch unregister(Name)),
	    stopped
    end.

%%----------------------------------------------------------------------
%% Function:	port_name/1
%% Purpose:		converting the port from integer to atom in right format
%% Args:		Port (integer)
%% Returns:		portServer... (atom)
%%----------------------------------------------------------------------
port_name(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).

%%----------------------------------------------------------------------
%% Function:	loop/1
%% Purpose:		loop and on accepting incoming connections on the listening
%%				port spawning a proccess to handle the requst
%% Args:		ListenSock (integer)
%%----------------------------------------------------------------------
loop(ListenSock) ->
    io:format("listen loop"),
	{ok, Sock} = gen_tcp:accept(ListenSock),
	{ok, {{_Ip1,_Ip2,_Ip3,_Ip4}, _Port}} = inet:peername(Sock),
	% io:format("~p.~p.~p.~p:~p~n", [Ip1, Ip2, Ip3, Ip4, Port]),
	Handler = spawn(fun () ->
						handle_req(gen_tcp:recv(Sock, 0), Sock)
					end),
	gen_tcp:controlling_process(Sock, Handler),
	loop(ListenSock).

%%----------------------------------------------------------------------
%% Function:	handle_req/1
%% Purpose:		parse the request and request for a response 
%% 				accordingly.
%% Args:		Sock (integer)
%%----------------------------------------------------------------------
handle_req({ok, {http_request, Method, {_PathMode ,Path}, _Version}}, Sock) ->
	case (Method) of
		'GET' ->
			{Headers, Response} = web_server:get_resp({Method, <<"">>, Path}),
			respond_to_client(Sock, Headers, Response);
		'POST' ->
			Length = get_post_length(Sock),
			Params = get_params(Sock, Length),
			{Headers, Response} = web_server:get_resp({Method, Params, Path}),
			respond_to_client(Sock, Headers, Response);
		_ ->
			send_unsupported_error(Sock)
	end;
handle_req(SockError, Sock) ->
	io:format("Error in receiving: ~n~p~n~n", [SockError]),
	gen_tcp:close(Sock).

%%----------------------------------------------------------------------
%% Function:	get_post_length/1
%% Purpose:		calculating the length of the post
%% Args:		Sock (integer)
%% Returns:		length (integer)
%%----------------------------------------------------------------------
get_post_length(Sock) ->
 	case gen_tcp:recv(Sock, 0, 60000) of
		{ok, {http_header, _, 'Content-Length', _, Length}} ->
			list_to_integer(Length);
		{ok, {http_header, _, _Header, _, _}}  ->
			get_post_length(Sock)
	end.

%%----------------------------------------------------------------------
%% Function:	get_params/1
%% Purpose:		extracting the post params from the header
%% Args:		Sock (integer), Length (integer)
%% Returns:		Body (list|integer)
%%----------------------------------------------------------------------
get_params(Sock, Length) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, http_eoh} ->
			inet:setopts(Sock, [{packet, raw}]),
			{ok, Body} = gen_tcp:recv(Sock, Length),
			Body;
		_ ->
			get_params(Sock, Length)
	end.

%%----------------------------------------------------------------------
%% Function:	send_unsupported_error/1
%% Purpose:		send the error response with headers to the client
%% Args:		Sock (integer)
%%----------------------------------------------------------------------
send_unsupported_error(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST, GET\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock).

%%----------------------------------------------------------------------
%% Function:	respond_to_client/3
%% Purpose:		send the response with headers to the client
%% Args:		Sock (integer), Headers (list|String), Response (binary)
%%----------------------------------------------------------------------
respond_to_client(Sock, Headers, Response) ->
	gen_tcp:send(Sock, [Headers, Response]),
	gen_tcp:close(Sock).

start_listen(Port) ->
    Result = gen_tcp:listen(Port,[binary,
			 {packet, http},
			 {active, false}
			]),
    case Result of
	{ok, ListenSocket} ->
	    loop(ListenSocket);
	_other ->
	    io:fwrite("Port ~p in use, retrying in 5 seconds\n",[Port]),
	    receive
	    after 5000 ->
		    start_listen(Port)
	    end
    end.
	

