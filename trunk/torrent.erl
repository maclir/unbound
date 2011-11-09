%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%% 
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/0,init_loader/1]).
-export([start_link/1,init/1]).

%% =============================================================================
%% Torrent loader function that is responsible for opening the persistent 
%% storage and dynamically add all the torrents found into the supervisor.

start_link_loader() ->
    Self = self(),
    spawn_link(?MODULE,init_loader,[Self]),
    receive
	{ok,Pid} ->
	    {ok,Pid}
    after 100 ->
	    {error,time_out}
    end.

init_loader(Pid)->
    io:fwrite("Torrent Loader Started!\n"),
    Pid ! {ok,self()},

    %% Dummy torrent 1
    StartFunc = {torrent,start_link,[dummy_torrent_1]},
    ChildSpec = {torrent1,StartFunc,permanent,brutal_kill,worker,[torrent1]},
    supervisor:start_child(Pid,ChildSpec),

    %% Dummy torrent 2
    StartFunc2 = {torrent,start_link,[dummy_torrent_2]},
    ChildSpec2 = {torrent2,StartFunc2,permanent,brutal_kill,worker,[torrent2]},
    supervisor:start_child(Pid,ChildSpec2).

    %% For each record in the database do:

    %% StartFunc = {torrent,start_link,[Record]},
    %% ChildSpec = {Hash,StartFunc,permanent,brutal_kill,worker,[Hash]},
    %% supervisor:start_child(Pid,ChildSpec).

    %% End of for each loop.


%% =============================================================================
%% Regular torrent functions

start_link(Var) ->
    {ok,spawn_link(torrent,init,[Var])}.

init(Var) ->
    io:fwrite("~p started\n",[Var]),
    loop().

loop() ->
    receive
	{test} ->
	    ok
    end.
