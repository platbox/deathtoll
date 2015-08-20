%%
%% Error logger alarmist

-module(deathtoll_alarm_logger).
-behaviour(deathtoll_alarmist).

-export([
    init/2,
    alarm/3,
    terminate/2
]).

-type state() :: undefined.

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, _Options) ->
    {ok, undefined}.

-spec alarm(deathtoll:cref(), deathtoll:alarm(), state()) -> {ok, state()}.

alarm(Ref, Alarm, State) ->
    _ = error_logger:error_msg(deathtoll_plain_formatter:format_alarm(Ref, Alarm)),
    {ok, State}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.
