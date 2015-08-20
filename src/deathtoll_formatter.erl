%%
%% Default alert-to-text formatter

-module(deathtoll_formatter).

-export([
    format_alarm/2,
    format_alarm/3
]).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm()) ->
    binary().

format_alarm(Ref, Alarm) ->
    format_alarm(Ref, Alarm, undefined).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm(), WasAlarm :: undefined | deathtoll:alarm()) ->
    binary().

format_alarm(Ref, Alarm = {down, #{since := Since}}, WasAlarm) ->
    Title = deathtoll_format:format_ref(Ref),
    Time = format_since(Since),
    Infix = case WasAlarm of
        {down, _} -> "still down since";
        _         -> "went down at"
    end,
    genlib:format(
        "Seems that ~s ~s ~s, you'd better check what's up. ~s",
        [Title, Infix, Time, deathtoll_auditor:format(Alarm, {text, plain})]
    );

format_alarm(Ref, Alarm = {up, _}, _WasAlarm) ->
    genlib:format(
        "Seems that ~s went back up, cheers! ~s",
        [deathtoll_format:format_ref(Ref), deathtoll_auditor:format(Alarm)]
    ).

format_since(Datetime) ->
    format_datetime(calendar:universal_time_to_local_time(Datetime)).

format_datetime(Datetime) ->
    genlib_format:format_datetime([h, $:, m], Datetime).
