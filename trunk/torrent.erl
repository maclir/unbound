%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%% 
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/0,init_loader/1]).
-export([start_link/1,init/1]).
-include("torrent_db_records.hrl").

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

    RecordList = torrent_db:size_gt(0),
    start_torrent(Pid,RecordList).
    
%%    %% Dummy torrent 1
%%    StartFunc = {torrent,start_link,[dummy_torrent_1]},
%%    ChildSpec = {torrent1,StartFunc,permanent,brutal_kill,worker,[torrent1]},
%%    supervisor:start_child(Pid,ChildSpec),

 
start_torrent(Pid,[Record|Tail]) -> 
    InfoHash = Record#torrent.info_sha,
    StartFunc = {torrent,start_link,[InfoHash]},
    ChildSpec = {InfoHash,StartFunc,permanent,brutal_kill,worker,[InfoHash]},
    supervisor:start_child(Pid,ChildSpec),
    start_torrent(Pid,Tail);

start_torrent(_Pid,[]) ->
    ok.

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
