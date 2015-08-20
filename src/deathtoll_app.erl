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
    {ok, _} = application:ensure_all_started(App, permanent),
    ok.

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
    supervisor:start_link(?MODULE, deathtoll_sup).

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

-spec init(_) -> {ok, {Strategy, Specs}} when
    Strategy :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()},
    Specs :: list(supervisor:child_spec()).

init(deathtoll_sup) ->
    WebSup = get_child_spec(web_sup, {{0, 0, 0, 0}, 8888, []}),
    WatchSuperSup = get_child_spec(watch_supersup, {}),
    {ok, {{one_for_one, 1, 30}, [WatchSuperSup, WebSup]}};

init(watch_supersup) ->
    Watches = genlib_opts:get(watch, application:get_all_env(), []),
    {ok, {{one_for_one, 6, 30}, [get_child_spec(watch, Watch) || Watch = {_, _} <- Watches]}}.

%%

get_child_spec(watch_supersup, _) ->
    {watch_supersup, {supervisor, start_link, [{local, ?MODULE}, ?MODULE, watch_supersup]},
        permanent, infinity, supervisor, [?MODULE]};

get_child_spec(web_sup, {IP, Port, Modules}) ->
    Routes = lists:flatmap(fun (M) -> M:routes() end, Modules),
    ranch:child_spec(
        ?MODULE, 8,
        ranch_tcp, [{ip, IP}, {port, Port}],
        cowboy_protocol, [{env, [{dispatch, cowboy_router:compile([{'_', Routes}])}]}]
    );

get_child_spec(watch, {Ref, Options}) ->
    {Ref, {deathtoll_watch_sup, start_link, [Options#{ref => Ref}]},
        permanent, infinity, supervisor, [deathtoll_watch_sup]}.
