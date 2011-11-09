%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(port).
-export([start_link_loader/0,init_loader/1]).
-export([start_link/0,init/0]).

%% =============================================================================

start_link_loader() ->
    Self = self(),
    spawn(?MODULE,init_loader,[Self]),
    receive
	{ok,Pid} ->
	    {ok,Pid}
    after 100 ->
	    {error,time_out}
    end.

init_loader(Pid) ->
    io:fwrite("Port loader started\n"),
    Pid ! {ok,self()},
    StartFunc = {port,start_link,[]},
    ChildSpec = {port1,StartFunc,permanent,brutal_kill,worker,[port1]},
    supervisor:start_child(Pid,ChildSpec).

%% =============================================================================

start_link()->
    {ok,spawn_link(?MODULE,init,[])}.

init() ->
    io:fwrite("Port opened\n"),
    loop().

loop()->
    receive
	{test} ->
	    ok
    end.

