-module(uploadcare_api).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	info/1,
	store/1,
	delete/1
]).

-define(TIMEOUT, 1000 * 30).
-define(CONNECT_TIMEOUT, 1000 * 15).
-define(FILE_URL(UUID), "https://api.uploadcare.com/files/" ++ UUID ++ "/").

%% ===================================================================
%% API functions
%% ===================================================================

info(UUID) ->
	request(get, ?FILE_URL(UUID), uploadcare_srv:get_headers()).

store(UUID) ->
	request(put, ?FILE_URL(UUID) ++ "storage/", uploadcare_srv:get_headers()).

delete(UUID) ->
	request(delete, ?FILE_URL(UUID), uploadcare_srv:get_headers()).

%% ===================================================================
%% Internal functions
%% ===================================================================

request(put, Path, Headers) ->
	{ok, {_, _, Response}} = httpc:request(put,
		{Path, Headers, "application/json", ""},
		[{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}], []),
	Response;

request(Method, Path, Headers) ->
	{ok, {_, _, Response}} = httpc:request(Method,
		{Path, Headers},
		[{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}], []),
	Response.
