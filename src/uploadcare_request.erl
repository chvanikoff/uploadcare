-module(uploadcare_request).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([do/2]).

-define(REQ_OPTS, [
	{timeout, 1000 * 30},
	{connect_timeout, 1000 * 15}
]).

%% ===================================================================
%% API functions
%% ===================================================================

do(Method, Path) ->
	do(Method, Path, uploadcare_srv:get_headers()).

%% ===================================================================
%% Internal functions
%% ===================================================================

do(put, Path, Headers) ->
	{ok, {_, _, Response}} = httpc:request(put,
		{Path, Headers, "application/json", ""},
		?REQ_OPTS, []),
	Response;

do(Method, Path, Headers) ->
	{ok, {_, _, Response}} = httpc:request(Method,
		{Path, Headers},
		?REQ_OPTS, []),
	Response.
