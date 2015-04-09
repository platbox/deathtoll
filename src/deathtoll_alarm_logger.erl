%%
%% Error logger alarmist

-module(deathtoll_alarm_logger).
-behaviour(deathtoll_alarmist).

-export([
    init/2,
    alarm/3,
    terminate/2
]).

-type state() :: {undefined | deathtoll:alarm(), module()}.

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, Options) ->
    {ok, {undefined, maps:get(formatter, Options, deathtoll_formatter)}}.

-spec alarm(deathtoll:cref(), deathtoll:alarm(), state()) -> {ok, state()}.

alarm(Ref, Alarm, {WasAlarm, Formatter}) ->
    _ = error_logger:error_msg(Formatter:format_alarm(Ref, Alarm, WasAlarm)),
    {ok, {Alarm, Formatter}}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.
