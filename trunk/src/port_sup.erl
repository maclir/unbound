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

-export([start_link/1]).
-export([init/1]).

%%----------------------------------------------------------------------
%% Function:   start_link/1
%% Purpose:
%% Args:       ClientId(string)
%% Returns:
%%----------------------------------------------------------------------
start_link(ClientId) ->
    supervisor:start_link(?MODULE,[ClientId]).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose:
%% Args:     ClientId(string)
%% Returns:
%%----------------------------------------------------------------------
init(ClientId) ->
    io:fwrite("Port Supervisor started\n"),
    {ok,{{one_for_one,1,10},
	 [{port,{tcp,init_listening,[6991,ClientId]},
	   transient,brutal_kill,worker,[port]}
	 ]
	}
    }.
