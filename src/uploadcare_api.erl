-module(uploadcare_api).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	info/1,
	store/1,
	delete/1
]).

-define(FILE_URL(UUID), "https://api.uploadcare.com/files/" ++ UUID ++ "/").

%% ===================================================================
%% API functions
%% ===================================================================

info(UUID) ->
	uploadcare_request:do(get, ?FILE_URL(UUID)).

store(UUID) ->
	uploadcare_request:do(put, ?FILE_URL(UUID) ++ "storage/").

delete(UUID) ->
	uploadcare_request:do(delete, ?FILE_URL(UUID)).
