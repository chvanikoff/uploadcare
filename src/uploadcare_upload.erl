-module(uploadcare_upload).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	upload_url/1,
	status/1
]).

-define(UPLOAD_URL(Type), "https://upload.uploadcare.com/" ++ Type ++ "/").

%% ===================================================================
%% API functions
%% ===================================================================

upload_url(URL) ->
	{M,S,_} = erlang:now(),
	Timestamp = M * 1000000 + S,
	Req_URL = ?UPLOAD_URL("from_url")
		++ "?_=" ++ integer_to_list(Timestamp)
		++ "&source_url=" ++ URL
		++ "&pub_key=" ++ uploadcare_srv:get_pubkey(),
	Response = uploadcare_request:do(get, Req_URL),
	case re:run(Response, "\:\s?\"([^\"]*)", [{capture, all_but_first, list}]) of
		{match, [Token]} ->
			Token;
		_ -> {error, Response}
	end.

status(Token) ->
	Req_URL = ?UPLOAD_URL("status") ++ "?token=" ++ Token,
	uploadcare_request:do(get, Req_URL).
