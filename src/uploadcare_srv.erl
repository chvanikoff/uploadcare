-module(uploadcare_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	set_auth_data/1,
	get_headers/0,
	get_pubkey/0
]).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	headers = [],
	pubkey = ""
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_auth_data(Data) ->
	gen_server:cast(?MODULE, {set_auth_data, Data}).

get_headers() ->
	gen_server:call(?MODULE, headers).

get_pubkey() ->
	gen_server:call(?MODULE, pubkey).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, #state{}}.


handle_call(headers, _From, #state{headers = Headers} = State) ->
	{reply, Headers, State};

handle_call(pubkey, _From, #state{pubkey = Pubkey} = State) ->
	{reply, Pubkey, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast({set_auth_data, Data}, State) ->
	Pub = proplists:get_value(public_key, Data, "no_pub"),
	Priv = proplists:get_value(private_key, Data, "no_priv"),
	Headers = [
		{"Authorization", "Uploadcare.Simple " ++ Pub ++ ":" ++ Priv},
		{"User-Agent", "Erlang uploadcare module"},
		{"Date", date:format("Y-m-d H:i:s")},
		{"Accept", "application/vnd.uploadcare-v0.2+json"}
	],
	{noreply, State#state{headers = Headers, pubkey = Pub}};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
