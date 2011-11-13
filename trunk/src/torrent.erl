%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/3,init/1]).
-include("torrent_db_records.hrl").

%% =============================================================================
%% Torrent loader function that is responsible for opening the persistent
%% storage and dynamically add all the torrents found into the supervisor.

start_link_loader(Id) ->
    Self = self(),
    spawn_link(?MODULE,init_loader,[{Self,Id}]),
    receive
	{ok,Pid} ->
	    {ok,Pid}
    after 100 ->
	    {error,time_out}
    end.

init_loader({Pid,Id})->
    io:fwrite("Torrent Loader Started!\n"),
    Pid ! {ok,self()},
    RecordList = torrent_db:size_gt(0),
    start_torrent(Pid,RecordList,Id).

start_torrent(Pid,[Record|Tail],Id) ->
    InfoHash = info_hash:to_hex(Record#torrent.info_sha),
    StartFunc = {torrent,start_link,[InfoHash,Id,Record]},
    ChildSpec = {InfoHash,StartFunc,permanent,brutal_kill,worker,[torrent]},
    supervisor:start_child(Pid,ChildSpec),
    start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
    ok.



%% =============================================================================
%% Regular torrent functions

start_link(Var,Id,Record) ->
    {ok,spawn_link(torrent,init,[{Var,Id,Record}])}.

init({Var,Id,Record}) ->
    InfoHashUrl = info_hash:url_encode(Record#torrent.info_sha),
    Announce = Record#torrent.announce,
    PeerList = tcp:connect_to_server(Announce,InfoHashUrl,Id),    
    io:fwrite("~p started by client ~p\n",[Var,Id]),
    loop().

loop() ->
    receive
	{test} ->
	    ok
    end.
