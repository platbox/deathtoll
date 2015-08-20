%%
%% Default alert-to-text formatter

-module(deathtoll_plain_formatter).

-export([format_alarm/2]).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm()) ->
    binary().

format_alarm(Ref, Alarm = {down, #{since := Since, n := N}}) ->
    Title = deathtoll_format:format_ref(Ref),
    Time = format_since(Since),
    Infix = case N of
        1 -> "went down at";
        _ -> "still down since"
    end,
    genlib:format(
        "Seems that ~s ~s ~s, you'd better check what's up. ~s",
        [Title, Infix, Time, deathtoll_auditor:format(Alarm, {text, plain})]
    );

format_alarm(Ref, Alarm = {up, _}) ->
    genlib:format(
        "Seems that ~s went back up, cheers! ~s",
        [deathtoll_format:format_ref(Ref), deathtoll_auditor:format(Alarm)]
    ).

format_since(Datetime) ->
    format_datetime(calendar:universal_time_to_local_time(Datetime)).

format_datetime(Datetime) ->
    genlib_format:format_datetime([h, $:, m], Datetime).
