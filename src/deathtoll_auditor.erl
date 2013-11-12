%%
%% Auditor behaviuor.

-module(deathtoll_auditor).

-type state() :: any().

-callback init(deathtoll:cref(), deathtoll:options()) ->
    {ok, state()}.

-callback start(deathtoll:cref(), state()) ->
    {alarm, deathtoll:alarm()}.

-callback terminate(deathtoll:cref(), state()) ->
    ok.

%%

-export([start_link/3]).
-export([init/4]).

-spec start_link(module(), state(), deathtoll:cref()) -> {ok, pid()}.

start_link(Module, State, Ref) ->
    proc_lib:start_link(?MODULE, init, [self(), Ref, Module, State], 5000).

-spec init(pid(), deathtoll:cref(), module(), state()) -> no_return().

init(Parent, Ref, Module, State) ->
    _ = proc_lib:init_ack(Parent, {ok, self()}),
    Alarm = Module:start(Ref, State),
    exit({shutdown, Alarm}).
