%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(app_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE,[]).

init(_Args) ->
    io:fwrite("Application Supervisor started!\n"),
    {ok,{{one_for_one,1,10},
	 [{torrent_mapper,{torrent_mapper,start_link,[]},
	   permanent, brutal_kill, worker,[torrent_mapper]},
	  {torrent_loader,{torrent,start_link_loader,[]},
	   transient, brutal_kill, worker,[torrent_loader]},
	  {port_sup,{port_sup,start_link,[]},
	   permanent, infinity, supervisor,[port_sup]},
	  {com_central,{com_central,start_link,[]},
	   permanent, brutal_kill, worker,[com_central]}
	 ]
	}
    }.

