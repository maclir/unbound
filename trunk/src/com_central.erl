%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(com_central).
-behaviour(gen_server).
-export([start_link/0]).
-export([start_download/0,add_new_torrent_file/1,add_new_torrent_url/1, get_all_torrents/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").


start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) ->
	io:fwrite("Com central started~n"),
    {ok,[]}.

start_download() ->
    gen_server:call(?MODULE, start_download).

add_new_torrent_url(Url) ->
    inets:start(),
    {ok, {_Status,_Headers,Body}} = httpc:request(get,{Url,[]},[],[]),
    add_new_torrent_file(list_to_binary(Body)).

add_new_torrent_file(Binary) ->
    gen_server:call(?MODULE, {add_new_torrent,Binary}).

get_all_torrents()->
    gen_server:call(?MODULE, {get_all_torrents}).

create_statuses([H|T], Statuses)->
    % torrent_mapper:req(H#torrent.info_sha),
    Info = H#torrent.info,
    % error here:
    create_statuses(T, [#torrent_status{info_hash=info_hash:to_hex(H#torrent.info_sha), priority=3, name="Dummy", size=torrent_db:get_size(H), status="Downloading", 
    peers = 4, downspeed = 34, upspeed = 640, eta = 101212, uploaded = 15}|Statuses]);
create_statuses([], Statuses) ->
    Statuses.

handle_call({add_new_torrent,Binary},_From,State) ->
    {ok,Record} = parser:decode(Binary),
    torrent_db:init(),
    torrent_db:add(Record),
    {reply,ok,State};
handle_call({remove_torrents, _}, _From, State)->
    {reply, removed, State};
handle_call({get_all_torrents}, _From, State) ->
    Torrents = torrent_db:get_all_torrents(),
    Statuses = create_statuses(Torrents, []),
    {reply, Statuses, State}.
    

handle_cast(_,_) ->
    ok.

handle_info(_,_) ->
    ok.

code_change(_,_,_) ->
    ok.

terminate(_,_) ->
    ok.
