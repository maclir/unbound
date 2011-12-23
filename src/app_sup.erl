%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(app_sup).
-behaviour(supervisor).
-export([start_link/0,stop/0,gen_random/1]).
-export([init/1]).

%%----------------------------------------------------------------------
%% Function:  start_link/0
%% Purpose:   creates client id and starts the main process of applciation
%%----------------------------------------------------------------------
start_link() ->
    case whereis(?MODULE) of
	undefined ->
	    inets:start(),
	    torrent_db:init(),
		receive
			after 100 ->
				ok
		end,
	    Id = clientId(),
	    supervisor:start_link({local,?MODULE},?MODULE,[Id]);
	Pid ->
	    io:fwrite("The Unbound Torrent client is already started with Pid ~p!",[Pid])
    end.

%%----------------------------------------------------------------------
%% Function:  init/1
%% Purpose:   spawns torrent_mapper(process that maps torrent pids to the
%%            related info hash), torrent_loader(process that spawns a new
%%            torrent process for each entry in the torrent database),port_sup(supervisor
%%            process that manage the listening port) and com_central(process
%%            that handles all the communications with a UI)
%% Args:      id (list)
%%----------------------------------------------------------------------
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
	   permanent, brutal_kill, worker,[com_central]},
	  {web_tcp,{web_tcp,start_link,[]},
	   permanent, brutal_kill, worker,[web_tcp]}
	 ]
	}
    }.

%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose:  stopes the applciation
%%----------------------------------------------------------------------
stop() ->
    lists:foreach(fun({Id,_,_,_})-> 
			  supervisor:terminate_child(unbound_torrent,Id) 
		  end,
		  supervisor:which_children(unbound_torrent)),
    exit(?MODULE,shutdown).


%%----------------------------------------------------------------------
%% Function:  get_random/2
%% Purpose:   To generate a random number with desired length
%% Args:      counter (integer),Binary(binary)
%% Returns:    random number
%%----------------------------------------------------------------------
gen_random(0, <<Binary/binary>>) ->
    Binary;
gen_random(Counter, <<Binary/binary>>)->
	RandomBinary = <<(random:uniform(256) - 1)>>,
	gen_random(Counter -1, <<RandomBinary/binary , Binary/binary>>).

%%----------------------------------------------------------------------
%% Function:  get_random/1
%% Purpose:   To initialize the generatin of a random number
%% Args:      counter (integer)
%% Returns:    random number
%%----------------------------------------------------------------------
gen_random(Counter) ->
	random:seed(erlang:now()),
	gen_random(Counter, <<>>).


%%----------------------------------------------------------------------
%% Function:   clientId/0
%% Purpose:    It generates a 20 character unique id client
%% Returns:    20 character unique ic client
%%----------------------------------------------------------------------
clientId() ->
	GeneralCode = <<"UB0001--">>,
	UniqueCode = gen_random(12),
	<<GeneralCode/binary, UniqueCode/binary>>.
