%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(com_central).
-behaviour(gen_server).
-export([start_link/1]).
-export([start_download/0,add_new_torrent_file/2,add_new_torrent_url/2]).
-export([torrent_command/2, get_all_torrents/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").


start_link(Id) ->
    gen_server:start_link({local,?MODULE},?MODULE,[Id],[{timeout,100000}]).

init([ClientId]) ->
    {ok,[{client_id,ClientId}]}.

start_download() ->
    gen_server:call(?MODULE, start_download).

add_new_torrent_url(Url, Path) ->
    inets:start(),
    {ok, {_Status,_Headers,Body}} = httpc:request(get,{Url,[]},[],[]),
    IsDir = filelib:is_dir(Path),
    case Path of
	"" ->
	    {ok, Dir} = file:get_cwd(),
	    FinalPath = Dir ++ "/Unbound_Dest/";
	_ when IsDir ->
	    FinalPath = Path;
	_ ->
	    {ok, Dir} = file:get_cwd(),
	    FinalPath = Dir ++ "/Unbound_Dest/"
    end,
    add_new_torrent_file(list_to_binary(Body), FinalPath).

add_new_torrent_file(Binary, Path) ->
    gen_server:call(?MODULE, {add_new_torrent,Binary, Path}).

torrent_command(Hash, Command) ->
    gen_server:call(?MODULE, {torrent_command,Hash,Command}).

get_all_torrents()->
    gen_server:call(?MODULE, {get_all_torrents}).

create_statuses([], Statuses) ->
    Statuses;
create_statuses([H|T], Statuses)->
    H ! {get_status_record, self()},
	io:fwrite("call:, ~p~n", [H]),
    receive 
        {status, Status} ->
            create_statuses(T, 
			    [Status|Statuses]);
        _ ->
            Statuses
    after 5000 ->
			Statuses
    end.

handle_call({add_new_torrent,Binary, Path},_From,State) ->
    {ok,Record} = parser:decode(Binary),
    torrent_db:init(),
    case torrent_db:hash_exists(Record#torrent.info_sha) of
	false ->
	    NewRecord = Record#torrent{dir=Path},
	    torrent_db:add(NewRecord),
	    AppSupPid = whereis(app_sup),
	    {client_id,Id} = lists:keyfind(client_id,1,State),
	    InfoHash = info_hash:to_hex(Record#torrent.info_sha),
	    StartFunc = {torrent,start_link,[Id,NewRecord]},
	    ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	    supervisor:start_child(AppSupPid,ChildSpec),
	    {reply,ok,State};
	true ->
	    {reply,duplicate,State}
    end;

handle_call({torrent_command,Hash,Command},_From,State) ->
    {ok,Pid} = torrent_mapper:req(Hash),
    Pid ! {command, Command},
    {reply, ok, State};

handle_call({remove_torrents, _}, _From, State)->
    {reply, removed, State};

handle_call({get_all_torrents}, _From, State) ->
	TorrentPids = torrent_mapper:req_all(),
    Statuses = create_statuses(TorrentPids, []),
    {reply, Statuses, State}.
    

handle_cast(_,_) ->
    ok.

handle_info(_,_) ->
    ok.

code_change(_,_,_) ->
    ok.

terminate(_,_) ->
    ok.
