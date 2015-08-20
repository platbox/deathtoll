%%
%% Auditor behaviuor.

-module(deathtoll_auditor).

-type state() :: any().
-type ctype() ::
    {text, plain | html} |
    {application, json}.

-callback init(deathtoll:cref(), deathtoll:options()) ->
    {ok, state()}.

-callback start(deathtoll:cref(), state()) ->
    {alarm, deathtoll:alarm()}.

-callback terminate(deathtoll:cref(), state()) ->
    ok.

-callback format(deathtoll:alarm(), ctype()) ->
    term().

%%

-export([start_link/3]).
-export([init/4]).
-export([shoot/1]).
-export([format/2]).

-export_type([ctype/0]).

-spec start_link(module(), state(), deathtoll:cref()) -> {ok, pid()}.

start_link(Module, State, Ref) ->
    proc_lib:start_link(?MODULE, init, [self(), Ref, Module, State], 5000).

-spec init(pid(), deathtoll:cref(), module(), state()) -> no_return().

init(Parent, Ref, Module, State) ->
    _ = proc_lib:init_ack(Parent, {ok, self()}),
    _ = receive shoot -> ok after 5000 -> exit(orphaned) end,
    {alarm, {St, Opts}} = Module:start(Ref, State),
    exit({shutdown, {alarm, {St, Opts#{module => Module}}}}).

-spec shoot(pid()) -> reference().

shoot(Pid) ->
    MonRef = monitor(process, Pid),
    _ = Pid ! shoot,
    MonRef.

-spec format(deathtoll:alarm(), ctype()) -> term().

format(Alarm = {_, #{module := Module}}, CType) ->
    Module:format(Alarm, CType).
