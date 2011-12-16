%%% @author Peter Myllykoski <peter@UL30JT>, Yavor Paunov
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(com_central).
-behaviour(gen_server).
-export([start_link/1]).
-export([start_download/0,add_new_torrent_file/2,add_new_torrent_url/2,get_files/1]).
-export([torrent_command/2, get_all_torrents/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/1
%% Purpose:  spawns the server process
%% Args:     Id(string)
%%----------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link({local,?MODULE},?MODULE,[Id],[{timeout,100000}]).

%%----------------------------------------------------------------------
%% Function:   init/1
%% Purpose:     initializes the server
%% Args:       ClientId(string)
%%----------------------------------------------------------------------
init([ClientId]) ->
    {ok,[{client_id,ClientId}]}.

%%----------------------------------------------------------------------
%% Function:   get_files/1
%% Purpose:    API function
%% Args:       InfoHash(string)
%%----------------------------------------------------------------------
get_files(InfoHash) ->
    gen_server:call(?MODULE, {get_files, InfoHash}).

%%----------------------------------------------------------------------
%% Function:   start_download/0
%% Purpose:    API function
%%----------------------------------------------------------------------
start_download() ->
    gen_server:call(?MODULE, start_download).

%%----------------------------------------------------------------------
%% Function:   add_new_torrent_url/2
%% Purpose:    fetches the content of the provided url and tries to parse it as
%%             a torrent file. If it succeeds it adds it to the database.
%% Args:       Url(string),Path(string)
%%----------------------------------------------------------------------
add_new_torrent_url(Url, Path) ->
    inets:start(),
    case httpc:request(Url) of
	{ok, {_Status,_Headers,Body}} ->
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
	    add_new_torrent_file(list_to_binary(Body), FinalPath);
	{error, Reason} ->
	    {error, Reason}
    end.

%%----------------------------------------------------------------------
%% Function:   add_new_torrent_file/2
%% Purpose:    API function for adding a parsed torrent file to the database.
%% Args:       Binary(binary),Path(string)
%%----------------------------------------------------------------------
add_new_torrent_file(Binary, Path) ->
    gen_server:call(?MODULE, {add_new_torrent,Binary, Path}).

%%----------------------------------------------------------------------
%% Function:   torrent_command/2
%% Purpose:    API function.
%% Args:       Hash(string),Command(string)
%%----------------------------------------------------------------------
torrent_command(Hash, Command) ->
    gen_server:call(?MODULE, {torrent_command,Hash,Command}).

%%----------------------------------------------------------------------
%% Function:   get_all_torrents/0
%% Purpose:    API function.
%%----------------------------------------------------------------------
get_all_torrents()->
    gen_server:call(?MODULE, {get_all_torrents}).

%%----------------------------------------------------------------------
%% Function:   create_statuses/2
%% Purpose:    generates statuses record corresponding to each torrent record in
%%             the database
%% Args:       list, Statuses(record)
%%----------------------------------------------------------------------
create_statuses([], Statuses) ->
    Statuses;
create_statuses([H|T], Statuses)->
    H ! {get_status_record, self()},
    receive
        {status, Status} ->
            create_statuses(T,
			    [Status|Statuses]);
        _ ->
            Statuses
    after 5000 ->
			Statuses
    end.

%%----------------------------------------------------------------------
%% Function:   handle_call/3
%% Purpose:    call back function for the above API functions
%% Args:       tuple,From(),State()
%%----------------------------------------------------------------------
handle_call({get_files,InfoHash},_From,State) ->
	{ok,Pid} = torrent_mapper:req(InfoHash),
	Pid ! {get_files, self()},
	receive
		{files, Files} ->
			{reply,Files,State}
	end;

handle_call({add_new_torrent,Binary, Path},_From,State) ->
    case parser:decode(Binary) of
	{ok, Record} ->
	    torrent_db:init(),
	    case torrent_db:hash_exists(Record#torrent.info_sha) of
		false ->
		    NewRecord = Record#torrent{dir=Path},
		    torrent_db:add(NewRecord),
		    AppSupPid = whereis(app_sup),
		    {client_id,Id} = lists:keyfind(client_id,1,State),
		    InfoHash = info_hash:to_hex(NewRecord#torrent.info_sha),
		    StartFunc = {torrent,start_link,[Id,NewRecord]},
		    ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,
				 worker,[torrent]},
		    supervisor:start_child(AppSupPid,ChildSpec),
		    {reply,{result,ok},State};
		true ->
		    {reply,{error,duplicate},State}
	    end;
	_Other ->
	    {reply,{error,invalid_url},State}
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

%%----------------------------------------------------------------------
%% Function:   handle_cast/2
%%----------------------------------------------------------------------
handle_cast(_,_) ->
    ok.
%%----------------------------------------------------------------------
%% Function:   handle_info/2
%%----------------------------------------------------------------------
handle_info(_,_) ->
    ok.

%%----------------------------------------------------------------------
%% Function:   code_change/3
%% Purpose
%%----------------------------------------------------------------------
code_change(_,_,_) ->
    ok.

%%----------------------------------------------------------------------
%% Function:   terminate/2
%% Purpose:
%% Returns>
%%----------------------------------------------------------------------
terminate(_,_) ->
    ok.
