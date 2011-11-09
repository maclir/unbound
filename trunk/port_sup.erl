%%%-------------------------------------------------------------------
%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>
%%%-------------------------------------------------------------------
-module(port_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE,[]).

init(_Args) ->
    io:fwrite("Port Supervisor started\n"),
    {ok,{{one_for_one,1,10},
	 [{port_loader,{port,start_link_loader,[]},
	   transient,brutal_kill,worker,[port_loader]}
	 ]
	}
    }.
