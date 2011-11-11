%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		main module for the webserver
%%%----------------------------------------------------------------------
-module(server).
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
			Pid = spawn(fun () ->
							{ok, ListenSock} = gen_tcp:listen(Port,[
															 	binary,
																{packet, 0},
																{active, false}
															]), 
							loop(ListenSock)
						end),
			register(Name, Pid),
	    	{ok, Port};
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
	{ok, Sock} = gen_tcp:accept(ListenSock),
	Handler = spawn(fun () ->
						handle_req(Sock)
					end),
	gen_tcp:controlling_process(Sock, Handler),
	loop(ListenSock).

%%----------------------------------------------------------------------
%% Function:	handle_req/1
%% Purpose:		parse the request and request for a response 
%% 				accordingly.
%% Args:		Sock (integer)
%%----------------------------------------------------------------------
handle_req(Sock) ->
	{ok, Request} = gen_tcp:recv(Sock, 0),
	{Method, Path, _Ver, Params} = seperate_req(Request),
	{Headers, Response} = web_server:get_resp({Method, Params, Path}),
	respond_to_client(Sock, Headers, Response).

%%----------------------------------------------------------------------
%% Function:	respond_to_client/3
%% Purpose:		send the response with headers to the client
%% Args:		Sock (integer), Headers (list|String), Response (binary)
%%----------------------------------------------------------------------
respond_to_client(Sock, Headers, Response) ->
	gen_tcp:send(Sock, [Headers, Response]),
	gen_tcp:close(Sock).

%%----------------------------------------------------------------------
%% Function:	seperate_req/1
%% Purpose:		parsing the request and seperating the different 
%%				elements
%% Args:		Resp (list|string)
%% Returns:		{Method, Path, Ver, PostData}
%%----------------------------------------------------------------------
seperate_req(Str) ->
	Sep = re:split(Str, "\r\n"),
	FirstLine = re:split(hd(Sep), " "),
	Method = hd(FirstLine),
	Path = hd(tl(FirstLine)),
	Ver = lists:last(FirstLine),
	{Method, Path, Ver, lists:last(Sep)}.