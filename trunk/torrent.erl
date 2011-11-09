%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/0,init_loader/1]).
-export([start_link/0,init/0]).

%% =============================================================================
%% Torrent loader function that dynamically loads all torrents and adds them to
%% the torrent supervisor list.

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
    StartFunc = {torrent,start_link,[]},
    ChildSpec = {torrent1,StartFunc,permanent,brutal_kill,worker,[torrent1]},
    supervisor:start_child(Pid,ChildSpec),
    StartFunc2 = {torrent,start_link,[]},
    ChildSpec2 = {torrent2,StartFunc2,permanent,brutal_kill,worker,[torrent2]},
    supervisor:start_child(Pid,ChildSpec2).

    %% For each record in the database do:

    %% StartFunc = {torrent,start_link,[Record]},
    %% ChildSpec = {Hash,StartFunc,permanent,brutal_kill,worker,[Hash]},
    %% supervisor:start_child(Pid,ChildSpec).

    %% End of for each loop.


%% =============================================================================
%% Regular torrent functions

start_link() ->
    {ok,spawn_link(torrent,init,[])}.

init() ->
    io:fwrite("Torrent started\n"),
    loop().

loop() ->
    receive
	{test} ->
	    ok
    end.
