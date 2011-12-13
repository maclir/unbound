%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(com_central).
-behaviour(gen_server).
-export([start_link/0]).
-export([start_download/0,add_new_torrent_file/1,add_new_torrent_url/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("torrent_db_records.hrl").


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
   
handle_call({add_new_torrent,Binary},_From,State) ->
    {ok,Record} = parser:decode(Binary),
    torrent_db:init(),
    torrent_db:add(Record),
    {reply,ok,State};
handle_call({get_all_torrents}, _From, State) ->
    Torrents = [].
    % TODO: Populate list with #torrent_status record
    {reply, Torrents,State}.
    

handle_cast(_,_) ->
    ok.

handle_info(_,_) ->
    ok.

code_change(_,_,_) ->
    ok.

terminate(_,_) ->
    ok.
