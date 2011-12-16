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
-export([reg/1,free/1,req_all/0,req/1]).
-export([init/1, handle_call/3]).

%%----------------------------------------------------------------------
%% Function:  start_link/0
%% Purpose:   to spawn gen_server
%% Returns:   {ok,Pid}
%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).


%%----------------------------------------------------------------------
%% Function:     reg/1
%% Purpose:      API function for registering new torrent process
%% Args:          SHA1(string)
%%----------------------------------------------------------------------
reg(SHA1) ->
    gen_server:call(?MODULE,{reg,SHA1}).


%%----------------------------------------------------------------------
%% Function:    free/1
%% Purpose:     API function for unregistering a torrent process
%% Args:         SHA1(string)

%%----------------------------------------------------------------------
free(SHA1) ->
    gen_server:call(?MODULE,{free,SHA1}).



%%----------------------------------------------------------------------
%% Function:     req/1
%% Purpose:      API function for registering new torrent process
%% Args:          SHA1(string)
%%----------------------------------------------------------------------
req(SHA1) ->
    gen_server:call(?MODULE,{req,SHA1}).
%%----------------------------------------------------------------------
%% Function:  req_all/0
%% Purpose:   API function
%% Returns:   a list of all the registered torrents
%%----------------------------------------------------------------------
req_all() ->
    gen_server:call(?MODULE,{req,all}).


%%----------------------------------------------------------------------
%% Function:     init/1
%% Purpose:      initializes the gen_server
%%----------------------------------------------------------------------
init(_Args) ->
    io:fwrite("Torrent Mapper started!\n"),
    {ok,[]}.

%%----------------------------------------------------------------------
%% Function:  handle_call/3
%% Purpose:   call back functions for gen_server
%% Args:      Action to be taken(tuple),From(tuple),Map(list of tuples)
%% Returns:   {reply,Reply,State} or {noreply,State}
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function:   free/3
%% Purpose:    removes all tuples containing the SHA1
%% Args:       SHA1(string),[{ID,Pid}|T](list),NewMap(list of tuples)
%% Returns:    NewMap
%%----------------------------------------------------------------------
free(SHA1,[{ID,Pid}|T],NewMap) ->
    case SHA1 of
	ID ->
	    free(SHA1,T,NewMap);
	_ ->
	    free(SHA1,T,[{ID,Pid}|NewMap])
    end;

free(_SHA1,[],NewMap) ->
    NewMap.

%%----------------------------------------------------------------------
%% Function:    req/2
%% Purpose:     return the pid handling given SHA1
%% Args:        SHA1(string),[{ID,Pid}|T](list)
%% Returns:      {ok,Pid}
%%----------------------------------------------------------------------
req(SHA1,[{ID,Pid}|T]) ->
    case SHA1 of
	ID ->
	    {ok,Pid};
	_ ->
	    req(SHA1,T)
    end;
req(_SHA1,[]) ->
    {error,not_found}.


%%----------------------------------------------------------------------
%% Function:    del_hash/2
%% Purpose:     returns the pid found in the tuple
%% Args:        SHA1(string), Pid(pid)
%% Returns:      Pid
%%----------------------------------------------------------------------
del_hash({_id,Pid}) ->
    Pid.
