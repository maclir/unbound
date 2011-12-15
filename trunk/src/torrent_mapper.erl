%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%% Server that maps info_hash requests to the corresponding 
%%% process id.
%%% @end
%%% Created :  8 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent_mapper).
-behavoiur(gen_server).
-export([start_link/0]).
-export([reg/1,free/1,req/1,req_all/0]).
-export([init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

reg(SHA1) ->
    gen_server:call(?MODULE,{reg,SHA1}).

free(SHA1) ->
    gen_server:call(?MODULE,{free,SHA1}).

req(SHA1) ->
    gen_server:call(?MODULE,{req,SHA1}).

req_all() ->
    gen_server:call(?MODULE,{req,all}).

init(_Args) ->
    io:fwrite("Torrent Mapper started!\n"),
    {ok,[]}.

handle_call({reg,SHA1},{Pid,_Tag},Map) ->
    NewMap = lists:keystore(SHA1,1,Map,{SHA1,Pid}),
    {reply,ok,NewMap};

handle_call({free,SHA1},_From,Map) ->
    NewMap = free(SHA1,Map,[]),
    {reply,ok,NewMap};

handle_call({req,all},_From,Map) ->
    Result = lists:map(fun(X)->del_hash(X) end,Map),
    {reply,Result,Map};

handle_call({req,SHA1},_From,Map) ->
    Result = req(SHA1,Map),
    {reply,Result,Map};

handle_call(_Message,_From,Map) ->
    {noreply,Map}.

free(SHA1,[{ID,Pid}|T],NewMap) ->
    case SHA1 of
	ID ->
	    free(SHA1,T,NewMap);
	_ ->
	    free(SHA1,T,[{ID,Pid}|NewMap])
    end;

free(_SHA1,[],NewMap) ->
    NewMap.

req(SHA1,[{ID,Pid}|T]) ->
    case SHA1 of
	ID ->
	    {ok,Pid};
	_ ->
	    req(SHA1,T)
    end;
req(_SHA1,[]) ->
    {error,not_found}.

del_hash({_id,Pid}) ->
    Pid.
