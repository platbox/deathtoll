%%
%% Watch supervisor

-module(deathtoll_watch_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

%%

-spec start_link(deathtoll:options()) -> {ok, pid()} | {error, any()}.

start_link(Options) ->
    try
        {ok, SupPid} = supervisor:start_link(?MODULE, watch_sup),
        AuditSupSpec = get_audit_sup_spec(Options),
        {ok, AuditSupPid} = supervisor:start_child(SupPid, AuditSupSpec),
        WatchSpec = get_watch_spec(Options#{sup => AuditSupPid}),
        {ok, _WatchPid} = supervisor:start_child(SupPid, WatchSpec),
        {ok, SupPid}
    catch
        error:{badmatch, Error = {error, _}} ->
            Error;
        error:{badmatch, Reason} ->
            {error, Reason};
        _:Error ->
            Error
    end.

get_audit_sup_spec(Options) ->
    Ref = maps:get(ref, Options),
    {Mod, Opts} = maps:get(auditor, Options),
    {ok, State} = Mod:init(Ref, Opts),
    Entry = {supervisor, start_link, [?MODULE, {audit_sup, Mod, State}]},
    {audit_sup, Entry, permanent, infinity, supervisor, [?MODULE, Mod]}.

get_watch_spec(Options) ->
    Ref = maps:get(ref, Options),
    Entry = {deathtoll_watch, start_link, [Ref, Options]},
    {watch, Entry, permanent, 10000, worker, [deathtoll_watch]}.

-spec init(watch_sup | {audit_sup, module(), any()}) -> {ok, {Strategy, Specs}} when
    Strategy :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()},
    Specs :: list(supervisor:child_spec()).

init(watch_sup) ->
    {ok, {{rest_for_one, 10, 10}, []}};

init({audit_sup, Module, State}) ->
    {ok, {{simple_one_for_one, 6, 30}, [
        {auditor, {deathtoll_auditor, start_link, [Module, State]},
            temporary, 5000, worker, [deathtoll_auditor, Module]}
    ]}}.
