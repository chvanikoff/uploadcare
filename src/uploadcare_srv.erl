-module(uploadcare_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	start_link/0,
	set_auth_data/1
]).

-record(state, {headers = []}).

-define(TIMEOUT, 1000 * 30).
-define(CONNECT_TIMEOUT, 1000 * 15).
-define(FILE_URL(UUID), "https://api.uploadcare.com/files/" ++ UUID ++ "/").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_auth_data(Data) ->
	gen_server:cast(?MODULE, {set_auth_data, Data}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, #state{}}.


handle_call({store, UUID}, _From, #state{headers = Headers} = State) ->
	{reply, fun() -> request(put, ?FILE_URL(UUID) ++ "storage/", Headers) end, State};

handle_call({info, UUID}, _From, #state{headers = Headers} = State) ->
	{reply, fun() -> request(get, ?FILE_URL(UUID), Headers) end, State};

handle_call({delete, UUID}, _From, #state{headers = Headers} = State) ->
	{reply, fun() -> request(delete, ?FILE_URL(UUID), Headers) end, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast({set_auth_data, Data}, State) ->
	{noreply, State#state{headers = get_headers(Data)}};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_headers(Auth_data) ->
	Public_key = proplists:get_value(public_key, Auth_data, "no_pub"),
	Private_key = proplists:get_value(private_key, Auth_data, "no_priv"),
	[
		{"Authorization", "Uploadcare.Simple " ++ Public_key ++ ":" ++ Private_key},
		{"User-Agent", "Erlang uploadcare module"},
		{"Date", date:format("Y-m-d H:i:s")},
		{"Accept", "application/vnd.uploadcare-v0.2+json"}
	].

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
