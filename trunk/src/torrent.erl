%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/3,init/1]).
-export([create_blocklist/2]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").

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
	ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	supervisor:start_child(Pid,ChildSpec),
	start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
	ok.



%% =============================================================================
%% Regular torrent functions

start_link(Var,Id,Record) ->
	{ok,spawn_link(torrent,init,[{Var,Id,Record}])}.

init({Var,Id,Record}) ->
	%% Check integrity of downloaded pieces, create a bitfield according to
	%% the result of the integrity check.
	NumPieces = byte_size((Record#torrent.info)#info.pieces) div 20,
	NumBlocks = (Record#torrent.info)#info.piece_length div 16384,
	OurBitfield = peerpiecemanagement:create_dummy_bitfield(NumPieces),
	
	case Record#torrent.announce_list of
		%% If the tracker list is empty, only use the main tracker
		[] ->
			%% Start communication with tracker and peers
			case peerpiecemanagement:getPeerList(Record,Id) of
				{ok,Interval,PeerList} ->
					tcp:connect_to_peer(PeerList,Record#torrent.info_sha),
					io:fwrite("~p started by client ~p\n",[Var,Id]),
					loop(Record, #torrent_status{
									 db_bitfield=OurBitfield,
									 temp_bitfield=OurBitfield,
									 num_pieces=NumPieces,
									 num_blocks=NumBlocks
								});
				{error,Reason} ->
					io:fwrite("~p",[Reason])
			end;
		
		%% Should be changed so that all the trackers are queried, or should
		%% the other trackers be fallback trackers if there is no connection to
		%% the main one?
		AnnounceList ->
			io:fwrite("Torrent has a announce list"),
			case peerpiecemanagement:getPeerList(Record,Id) of
				{ok,Interval,PeerList} ->
					peerpiecemanagement:connect_to_peer(PeerList,Record#torrent.info_sha),
					io:fwrite("~p started by client ~p\n",[Var,Id]),
					loop(Record, #torrent_status{
										db_bitfield=OurBitfield,
										temp_bitfield=OurBitfield,
										num_pieces=NumPieces,
										num_blocks=NumBlocks
								});
				{error,Reason} ->
					io:fwrite("~p",[Reason])
			end
	end.
    
loop(Record, StatusRecord) ->
	receive
		{bitfield,Pid,Bitfield} ->
			NumPieces = StatusRecord#torrent_status.num_pieces,
			TempBitfield = StatusRecord#torrent_status.temp_bitfield,
			Index = peerpiecemanagement:get_index(TempBitfield,Bitfield,NumPieces),
			case get_blocklist(Index) of
				{result,not_found} ->
					NumBlocks = StatusRecord#torrent_status.num_blocks,
					BlockList = create_blocklist(Index,NumBlocks);
				{result,BlockList} ->
					ok
			end,
			BlockIndex=findblock(BlockList),
			Pid ! {piece , Index, BlockIndex * 16384 , 16384},
			TempBitfield =peerpiecemanagement:compare_bitfields(TempBitfield,Bitfield,NumPieces,Pid),
			loop(Record, StatusRecord#torrent_status{temp_bitfield=TempBitfield});
		
		{downloaded,PieceId, Offset, Data} -> 
		%%send request for new piece and proccess if done or not
		write_to_file:write(PieceId, Offset, Data, Record, Done);
		Msg ->
			io:fwrite("~p\n",[Msg]),
			loop(Record, StatusRecord)
	end.




create_blocklist(Index,NumBlocks) ->
	{Index ,fillblocklist(NumBlocks)}.

fillblocklist(0) ->
	[];
fillblocklist(NumBlocks) ->
	[0|fillblocklist(NumBlocks -1)].



findblock(BlockList) ->
	{_,List} = BlockList,
	iteratelist(0,List).

iteratelist(_,[])->
	ok;
iteratelist(Index,[H|T]) ->
	case H of
		0 ->
			Index;
		_ ->
			iteratelist(Index,T)
	end.



