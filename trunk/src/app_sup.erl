%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(app_sup).
-behaviour(supervisor).
-export([start_link/0,stop/0,gen_random/2]).
-export([init/1]).

start_link() ->
    random:seed(erlang:now()),
    case whereis(?MODULE) of
	undefined ->
	    inets:start(),
	    torrent_db:init(),
	    Id = clientId(),
	    supervisor:start_link({local,?MODULE},?MODULE,[Id]);
	Pid ->
	    io:fwrite("The Unbound Torrent client is already started with Pid ~p!",[Pid])
    end.


init([Id]) ->
    io:fwrite("Application Supervisor started!\n"),
    {ok,{{one_for_one,1,10},
	 [{torrent_mapper,{torrent_mapper,start_link,[]},
	   permanent, brutal_kill, worker,[torrent_mapper]},
	  {torrent_loader,{torrent,start_link_loader,[Id]},
	   transient, brutal_kill, worker,[torrent_loader]},
	  {port_sup,{port_sup,start_link,[Id]},
	   permanent, infinity, supervisor,[port_sup]},
	  {com_central,{com_central,start_link,[Id]},
	   permanent, brutal_kill, worker,[com_central]}
	 ]
	}
    }.

stop() ->    
    lists:foreach(fun({Id,_,_,_})-> supervisor:terminate_child(unbound_torrent,Id) end,supervisor:which_children(unbound_torrent)),
    exit(unbound_torrent,shutdown).

%% Function for generating a random number with desired length
gen_random(0, <<Binary/binary>>) ->
    Binary;
gen_random(Counter, <<Binary/binary>>)->
	RandomBinary = <<(random:uniform(256) - 1)>>,
	gen_random(Counter -1, <<RandomBinary/binary , Binary/binary>>).

%% Function for generating a 20 charachter unique id client
clientId() ->
	GeneralCode = <<"-UB0001-">>,
	UniqueCode = gen_random(12, <<>>),
	<<GeneralCode/binary, UniqueCode/binary>>.
