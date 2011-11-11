-module(hashcheck).

-export([calculate_hash/1, compare/2,is_equal/2]).

%% calcualtes the sha1 for the binary file
calculate_hash(File) ->
     crypto:sha(File).


%% first calculates the sha1 for the received data and then sends it together
%% with the sha1 stored in the file to comparator

compare(Sha1 , ReceivedData) ->
    HashFile = calculate_hash(ReceivedData),
    is_equal(Sha1 , HashFile).

%% comparing the sha1 files.

is_equal(Sha1 , Sha1) ->
    true;
is_equal(_,_) ->
    false.










