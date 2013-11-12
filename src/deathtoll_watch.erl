%%
%% Watch

-module(deathtoll_watch).
-behaviour(gen_server).

%%

-export([
    start_link/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    ref :: deathtoll:cref(),
    alarm :: deathtoll:alarm(),
    audit :: undefined | reference(),
    audit_sup :: pid(),
    timer :: reference(),
    alarmists :: [{module(), any()}],
    intervals :: {pos_integer(), pos_integer()},
    max_seq :: pos_integer(),
    max_alarms :: pos_integer()
}).

%%

-spec start_link(deathtoll:cref(), deathtoll:options()) -> {ok, pid()} | {error, any()}.

start_link(Ref, Options) ->
    AuditSup = deepprops:require(sup, Options),
    Alarmists = deepprops:require(alarms, Options),
    [Interval, MaxSeq, MaxAlarms] = deepprops:values([
        {interval, 600}, {max_seq, 3}, {max_alarms, 5}],
        Options
    ),
    IntervalDown = deepprops:get(interval_down, Options, Interval div 2),
    gen_server:start_link(?MODULE, #state{
        ref = Ref,
        alarm = {up, []},
        alarmists = Alarmists,
        intervals = {Interval * 1000, IntervalDown * 1000},
        max_seq = MaxSeq,
        max_alarms = MaxAlarms,
        audit_sup = AuditSup
    }, []).

%%

init(State = #state{ref = Ref, alarmists = Alarmists}) ->
    _ = process_flag(trap_exit, true),
    {ok, rearm(State#state{
        alarmists = [deathtoll_alarmist:init(Ref, Mod, Opts) || {Mod, Opts} <- Alarmists]
    }, 0)}.

handle_call(Call, _From, State) ->
    {stop, {error, {badcall, Call}}, State}.

handle_cast(Cast, State) ->
    {stop, {error, {badcast, Cast}}, State}.

handle_info({timeout, Ref, _Self}, State = #state{timer = Ref}) ->
    FinalState = handle_timeout(rearm(State#state{timer = undefined})),
    {noreply, FinalState};

handle_info({'DOWN', MonRef, process, _Pid, Info}, State = #state{audit = MonRef}) ->
    FinalState = handle_audit(Info, State#state{audit = undefined}),
    {noreply, FinalState};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, #state{ref = Ref, alarmists = Alarmists}) ->
    _ = [deathtoll_alarmist:terminate(Ref, A) || A <- Alarmists],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

rearm(State) ->
    rearm(State, get_timeout(State)).

rearm(State = #state{timer = undefined}, Timeout) ->
    State#state{timer = erlang:start_timer(Timeout, self(), self())}.

get_timeout(#state{alarm = {down, _}, intervals = {_, Timeout}}) ->
    Timeout;

get_timeout(#state{alarm = _, intervals = {Timeout, _}}) ->
    Timeout.

handle_timeout(State = #state{audit = undefined}) ->
    start_audit(State);

handle_timeout(State = #state{ref = Ref}) ->
    _ = error_logger:error_msg("~p: Ongoing audit takes too long", [Ref]),
    State.

start_audit(State = #state{ref = Ref, audit_sup = SupPid}) ->
    case supervisor:start_child(SupPid, [Ref]) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            State#state{audit = MonRef};
        Error ->
            _ = error_logger:error_msg("~p: Audit failed to start: ~p", [Ref, Error]),
            State
    end.

handle_audit({shutdown, {alarm, Alarm}}, State = #state{ref = Ref, audit = undefined, alarm = WasAlarm}) ->
    _ = error_logger:info_msg("~p: Audit completed: ~p", [Ref, Alarm]),
    case join_alarm(Alarm, WasAlarm, State) of
        {trigger, FinalAlarm} ->
            Alarmists = [deathtoll_alarmist:trigger(Ref, FinalAlarm, A) || A <- State#state.alarmists],
            State#state{alarm = FinalAlarm, alarmists = Alarmists};
        {ok, FinalAlarm} ->
            State#state{alarm = FinalAlarm}
    end;

handle_audit(Error, State = #state{ref = Ref, audit = undefined}) ->
    _ = error_logger:error_msg("~p: Audit failed to complete: ~p", [Ref, Error]),
    State.

join_alarm({up, Extra}, {up, _WasExtra}, _State) ->
    {ok, {up, Extra}};

join_alarm({up, Extra}, {down, _WasExtra}, _State) ->
    {trigger, {up, Extra}};

join_alarm({down, Extra0}, {up, _WasExtra}, #state{max_seq = MaxSeq}) ->
    Extra = [{seq, 1}, {since, calendar:universal_time()} | Extra0],
    if
        MaxSeq =:= 1 ->
            {trigger, {down, [{n, 1} | Extra]}};
        true ->
            {ok, {down, [{n, 0} | Extra]}}
    end;

join_alarm({down, Extra0}, {down, WasExtra}, #state{max_seq = MaxSeq, max_alarms = MaxN}) ->
    [N, Seq0, Since] = deepprops:values([n, seq, since], WasExtra),
    Seq = Seq0 + 1,
    TriggerSeq = MaxSeq + trunc(math:pow(2, N)) - 1,
    Extra = [{seq, Seq}, {since, Since} | Extra0],
    if
        Seq >= TriggerSeq andalso N < MaxN ->
            {trigger, {down, [{n, N + 1} | Extra]}};
        true ->
            {ok, {down, [{n, N} | Extra]}}
    end.
