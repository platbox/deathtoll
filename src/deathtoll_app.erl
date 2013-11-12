%%
%% Application behaviour implementation.

-module(deathtoll_app).
-behaviour(application).
-behaviour(supervisor).

%%

-export([
    start_app/1,
    stop_app/1
]).

-export([
    start_child/2,
    stop_child/1
]).

-export([start/2, stop/1]).
-export([start_link/0, init/1]).


%%

-spec start_app(atom()) -> ok.

start_app(App) ->
    do_start(App).

-spec stop_app(atom()) -> ok | {error, term()}.

stop_app(App) ->
    application:stop(App).

%%

-spec start(StartType, term()) -> {ok, pid()} when
    StartType :: normal | {takeover, node()} | {failover, node()}.

start(_StartType, _StartArgs) ->
    start_link().

-spec stop(term()) -> ok.

stop(_State) ->
    ok.

%%

-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(deathtoll:cref(), deathtoll:options()) -> {ok, pid()} | {error, {already_started, pid()} | any()}.

start_child(Ref, Options) ->
    supervisor:start_child(?MODULE, get_child_spec(Ref, Options)).

-spec stop_child(deathtoll:cref()) -> ok | {error, not_found}.

stop_child(Ref) ->
    case supervisor:terminate_child(?MODULE, Ref) of
        ok ->
            supervisor:delete_child(?MODULE, Ref);
        {error, Reason} ->
            {error, Reason}
    end.

%%

-spec init([]) -> {ok, {Strategy, Specs}} when
    Strategy :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()},
    Specs :: list(supervisor:child_spec()).

init([]) ->
    Watches = deepprops:get(watch, application:get_all_env(), []),
    {ok, {{one_for_one, 6, 30}, [get_child_spec(Ref, Options) || {Ref, Options} <- Watches]}}.

get_child_spec(Ref, Options) ->
    {Ref, {deathtoll_watch_sup, start_link, [[{ref, Ref} | Options]]},
        permanent, infinity, supervisor, [deathtoll_watch_sup]}.

%%

do_start(App) ->
    do_start(App, application:start(App, permanent)).

do_start(_, ok) ->
    ok;

do_start(_, {error, {already_started, _App}}) ->
    ok;

do_start(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = do_start(Dep),
    do_start(App);

do_start(App, {error, Reason}) ->
    erlang:error({app_startup_failed, App, Reason}).
