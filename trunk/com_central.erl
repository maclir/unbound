%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(com_central).
-behaviour(gen_server).
-export([start_link/0]).
-export([start_download/0]).
-export([init/1,handle_call/3]).


start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) ->
    {ok,[]}.

start_download() ->
    gen_server:call(?MODULE, start_download).

handle_call(Command, _From, State) ->
    io:fwrite("~p command was received!\n",[Command]),
    {reply,"Dummy response to download",State}.

    

