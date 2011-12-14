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
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").


start_link(Id) ->
    gen_server:start_link({local,?MODULE},?MODULE,[Id],[{timeout,100000}]).

init([ClientId]) ->
	io:fwrite("Com central started~n"),
    {ok,[{client_id,ClientId}]}.

start_download() ->
    gen_server:call(?MODULE, start_download).

add_new_torrent_url(Url, Path) ->
    inets:start(),
    {ok, {_Status,_Headers,Body}} = httpc:request(get,{Url,[]},[],[]),
	case Path of
		"" ->
			{ok, Dir} = file:get_cwd(),
			FinalPath = Dir ++ "/Unbound_Dest/";
		_ ->
			FinalPath = Path
	end,
    add_new_torrent_file(list_to_binary(Body), FinalPath).

add_new_torrent_file(Binary, Path) ->
    gen_server:call(?MODULE, {add_new_torrent,Binary, Path}).

%% get_all_torrents()->
%%     gen_server:call(?MODULE, {get_all_torrents}).

create_statuses([H|T], Statuses)->
    {ok, TPid} = torrent_mapper:req(H#torrent.info_sha),
    io:format("pid ~p\n", [TPid]),
    TPid ! {get_statistics, self()},
    receive 
        {statistics, Uploaded, Downloaded, Remaining} ->
            Info = H#torrent.info,
            create_statuses(T, 
			    [#torrent_status{info_hash=info_hash:to_hex(H#torrent.info_sha), 
					     priority=3, 
					     name=binary_to_list(Info#info.name), 
					     size=torrent_db:get_size_by_id(H#torrent.id), 
					     status="Downloading", 
					     peers = 4, 
					     downspeed = 0, 
					     upspeed = 0, 
					     eta = 101212, 
					     downloaded=Downloaded, 
					     uploaded=Uploaded}|Statuses]);

        _ ->
            Statuses
    after 5000 -> Statuses
    end;    % error here:
create_statuses([], Statuses) ->
    Statuses.

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

handle_call({remove_torrents, _}, _From, State)->
    {reply, removed, State}.

%% handle_call({get_all_torrents}, _From, State) ->
%%     Torrents = torrent_db:get_all_torrents(),
%%     Statuses = create_statuses(Torrents, []),
%%     {reply, Statuses, State}.
    

handle_cast(_,_) ->
    ok.

handle_info(_,_) ->
    ok.

code_change(_,_,_) ->
    ok.

terminate(_,_) ->
    ok.
