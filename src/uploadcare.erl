-module(uploadcare).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0, start/1,
	stop/0,
	set_auth_data/1,
	store/1,
	info/1,
	delete/1
]).

%% Application behaviour
-behaviour(application).
-export([start/2, stop/1]).

%% Supervisor behaviour
-behaviour(supervisor).
-export([init/1]).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(WORKER(I), ?CHILD(I, worker)).
-define(SUPERVISOR(I), ?CHILD(I, supervisor)).

-define(SERVER, uploadcare_srv).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	start([]).

start(Auth_data) ->
	ok = ensure_started(uploadcare),
	ok = case Auth_data of
		[] -> ok;
		_ -> set_auth_data(Auth_data)
	end,
	ok.

set_auth_data(Data) ->
	gen_server:cast(?SERVER, {set_auth_data, Data}).

stop() ->
	application:stop(tp).

store(UUID) ->
	gen_server:call(?SERVER, {store, UUID}).

info(UUID) ->
	gen_server:call(?SERVER, {info, UUID}).

delete(UUID) ->
	gen_server:call(?SERVER, {delete, UUID}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
	ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 5, 10},
	Children = [
		?WORKER(uploadcare_srv)
	],
	{ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started(App) when not is_list(App) -> ensure_started([App]);
ensure_started([]) -> ok;
ensure_started([App | Apps] = All) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps);
		{error, {not_started, Dependency}} -> ensure_started([Dependency | All])
	end.
