%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		responsible for handling the requests and
%%%				generating the response
%%%----------------------------------------------------------------------
-module(web_server).
-export([get_resp/1]).

%%----------------------------------------------------------------------
%% Function:	get_resp/3
%% Purpose:		handle the GET and POST requests
%% Args:		Method (<<"GET">> | <<"POST">>)
%% Returns:		{Header (list|string), Content (binary)}
%%----------------------------------------------------------------------
get_resp({'GET', _, Path}) ->
    {ok, Cwd} = file:get_cwd(),
	case Path of
		"/" ->
			FinalPath = Cwd ++ "/../UI"  ++ Path ++ "index.html";
		_ ->
			FinalPath = Cwd ++ "/../UI"  ++ Path
	end,			
	Content = get_content(FinalPath),
    Header = get_header(classify(Path), Content),
    {Header, Content};
get_resp({'POST', UnparsedParams, _Path}) ->
    Params = seperate_params(UnparsedParams),
    {Type , Content} = get_post_result(Params),
    Header = get_header(Type, Content),
    {Header, Content}.

%%----------------------------------------------------------------------
%% Function:	seperate_params/1
%% Purpose:		seperating the parameter lines from a POST
%% Args:		Str (list|string)
%%----------------------------------------------------------------------
seperate_params(Str) ->
	Sep = re:split(Str, "&"),
	seperate_each_param(Sep, []).

%%----------------------------------------------------------------------
%% Function:	seperate_each_param/2
%% Purpose:		seperating each line of the post parameters
%% Args:		Line (list), Data (list) 
%% Returns:		Data (list) (the parameters broken into a list) 
%%----------------------------------------------------------------------
seperate_each_param([], Data) ->
	Data;
seperate_each_param([H|T], Data) ->
	Sep = re:split(H, "="),
	seperate_each_param(T, [{hd(Sep), hd(tl(Sep))}|Data]).

%%----------------------------------------------------------------------
%% Function:	get_post_result/1
%% Purpose:		seperating and parsing the respnse for different POSTs
%% Args:		Data (list)
%% Returns:		{type (atom) (type of the response),
%%				Content(binary)(the response)}
%%----------------------------------------------------------------------
% get_post_result([{<<"qtype">>,_},
			% {<<"query">>,_},
			% {<<"sortorder">>,_},
			% {<<"sortname">>,_},
			% {<<"rp">>,_},
			% {<<"page">>,_}]) ->
	% {xml, web_response:get_data_xml({})};
get_post_result([{<<"filter">>, Filter}]) ->
	{xml, web_response:get_data_xml(Filter)};
%% Commands: resume, stop, delete
get_post_result([{<<"row">>, Row},
				 {<<"command">>,Command}]) ->
	io:format("Command: ~p, Row: ~p~n", [Command, Row]),
    com_central:torrent_command(info_hash:from_hex(binary_to_list(Row)), binary_to_atom(Command, utf8)),
	{text, <<"done">>};
%% Commands: new
get_post_result([{<<"url">>, Url},
				 {<<"command">>,Command}]) ->
	io:format("Command: ~p, Url: ~p~n", [Command, info_hash:url_decode(binary_to_list(Url))]),
	Result = com_central:add_new_torrent_url(Url, ""),
	case Result of
		ok ->
			{text, <<"done">>};
		duplicate ->
			{text, <<"torrent already exists">>};
		invalid_url ->
			{text, <<"invalud url">>};
		invalid_folder ->
			{text, <<"invalud folder path">>}
	end;

%% Commands: exit, settings
get_post_result([{<<"command">>,Command}]) ->
	io:format("Command: ~p~n", [Command]),
    case list_to_binary(Command) of
        "exit" ->
            app_sup:stop();
        "settings"->
            ok;
        _-> ok
    end,
	{text, <<"done">>};
get_post_result(Whatttt) ->
	io:format("post: ~p~n", [Whatttt]),
	{text, <<"done">>}.

%%----------------------------------------------------------------------
%% Function:	get_content/1
%% Purpose:		reading the file in Path and sending back the contents
%% Args:		Path (list|string)
%% Returns:		Bin (list) (file content) | "File does not exist" ...
%%				file has not been found in the path
%%----------------------------------------------------------------------
get_content(Path) ->
	case file:read_file(Path) of
		{ok, Bin} ->
			Bin;
		_ ->
			list_to_binary("File does not exist on this location: " ++ Path)
	end.

%%----------------------------------------------------------------------
%% Function:	classify/1
%% Purpose:		getting the file type of the requested file
%% Args:		FileName (list|string)
%% Returns:		(atom) gif | png | jpg | html | js | css | text
%%----------------------------------------------------------------------
classify(FileName) ->
	case filename:extension(FileName) of
		".GIF" -> gif;
		".gif" -> gif;
		".png" -> png;
		".PNG" -> png;
		".jpg" -> jpg;
		".JPG" -> jpg;
		".jpeg" -> jpg;
		".JPEG" -> jpg;
		".HTML" -> html;
		".html" -> html;
		".HTM" -> html;
		".htm" -> html;
		".JS" -> js;
		".js" -> js;
		".CSS" -> css;
		".css" -> css;
		_Other -> text
	end.

%%----------------------------------------------------------------------
%% Function:	get_header/2
%% Purpose:		generating the header based on the file type and 
%%				content size
%% Args:		file type (atom), Content (binary)
%% Returns:		Header (list|string)
%%----------------------------------------------------------------------
get_header(xml, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("application/xml") ++ content_size(Content);
get_header(js, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("text/javascript") ++ content_size(Content);
get_header(css, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("text/css") ++ content_size(Content);
get_header(text, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("text/html") ++ content_size(Content);
get_header(html, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("text/html") ++ content_size(Content);
get_header(jpg, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("image/jpeg") ++ content_size(Content);
get_header(gif, Content)  ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("image/gif") ++ content_size(Content);
get_header(png, Content) ->
	"HTTP/1.1 200 Ok\r\n" ++ powered_by() ++ content_type("image/png") ++ content_size(Content).

%%----------------------------------------------------------------------
%% Function:	powered_by/0
%% Purpose:		X-Powered-By for header
%% Returns:		PoweredBy (list|string)
%%----------------------------------------------------------------------
powered_by() ->
	"X-Powered-By: Erlang-Unbound \r\n".

%%----------------------------------------------------------------------
%% Function:	content_type/1
%% Purpose:		Content-Type for header
%% Returns:		ContentType (list|string)
%%----------------------------------------------------------------------
content_type(X) ->
	"Content-Type: " ++ X ++ "\r\n".

%%----------------------------------------------------------------------
%% Function:	content_size/1
%% Purpose:		Content-Length for header
%% Returns:		ContentLength (list|string)
%%----------------------------------------------------------------------
content_size(Content) ->
	"Content-Length: " ++ integer_to_list(size(Content)) ++ "\r\n\r\n".
