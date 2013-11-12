%%
%% Error logger alarmist

-module(deathtoll_alarm_logger).
-behaviour(deathtoll_alarmist).

-export([
    init/2,
    alarm/3,
    terminate/2
]).

-export([
    format_alarm/1,
    format_alarm/2
]).

-type state() :: undefined | deathtoll:alarm().

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, _Options) ->
    {ok, undefined}.

-spec alarm(deathtoll:cref(), deathtoll:alarm(), state()) -> {ok, state()}.

alarm(Ref, Alarm, WasAlarm) ->
    _ = error_logger:error_msg("Seems that ~p ~s ~n", [Ref, format_alarm(Alarm, WasAlarm)]),
    {ok, Alarm}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.

%%

-spec format_alarm(deathtoll:alarm()) -> string().

format_alarm(Alarm) ->
    format_alarm(Alarm, undefined).

-spec format_alarm(deathtoll:alarm(), WasAlarm :: undefined | deathtoll:alarm()) -> string().

format_alarm({down, _Extra}, {down, _}) ->
    "is still offline, it is better to check what's going on...";

format_alarm({down, _Extra}, _WasAlarm) ->
    "went offline, it is better to check what's going on...";

format_alarm({up, _Extra}, _WasAlarm) ->
    "went back online, cheers!".
