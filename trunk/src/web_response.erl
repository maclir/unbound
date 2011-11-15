%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		generating teh xml representing the state of
%%				the application
%%%----------------------------------------------------------------------
-module(web_response).
-export([get_xml/0]).

get_data_xml() ->
	Header = 
		"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
		<rows>
		<page>1</page>
		<total>1</total>",
	
get_xml() ->
	TempBit = lists:flatten(lists:map(fun(A) -> integer_to_list(A) ++ "-" end, binary_to_list(<<111,171,93>>))),
	Bit = string:sub_string(TempBit, 1, length(TempBit) - 1),

	Body = get_body(get_data(all_torrents)),

	Body = "
		
		++ "
		<row id=\"" ++ Bit ++ "-23\">
			<cell><![CDATA[" ++ "1" ++ "]]></cell>
			<cell><![CDATA[" ++ "illegal stuff" ++ "]]></cell>
			<cell><![CDATA[" ++ "1.13 GB" ++ "]]></cell>
			<cell><![CDATA[" ++ "20" ++ "]]></cell>
			<cell><![CDATA[" ++ "uploading" ++ "]]></cell>
			<cell><![CDATA[" ++ "2 (90)" ++ "]]></cell>
			<cell><![CDATA[" ++ "1(123)" ++ "]]></cell>
			<cell><![CDATA[" ++ "100" ++ "]]></cell>
			<cell><![CDATA[" ++ "23" ++ "]]></cell>
			<cell><![CDATA[" ++ "01:12:10" ++ "]]></cell>
			<cell><![CDATA[" ++ "100" ++ "]]></cell>
		</row>",
	
	Footer = "</rows>",
	Data = Header ++ Body ++ Footer,
	XmlData = lists:flatten(Data),
	iolist_to_binary(XmlData).

get_body([], ) ->
	