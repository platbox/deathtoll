%%
%% Default alert-to-text formatter

-module(deathtoll_formatter).

-export([
    format_alarm/2,
    format_alarm/3,
    format_ref/1
]).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm()) ->
    binary().

format_alarm(Ref, Alarm) ->
    format_alarm(Ref, Alarm, undefined).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm(), WasAlarm :: undefined | deathtoll:alarm()) ->
    binary().

format_alarm(Ref, {down, E}, {down, _}) ->
    Title = format_ref(Ref),
    Since = format_since(<<" since ">>, E),
    genlib:format("Seems that ~s still offline~s, you'd better check what's up.", [Title, Since]);

format_alarm(Ref, {down, E}, _WasAlarm) ->
    Title = format_ref(Ref),
    Since = format_since(<<" at ">>, E),
    genlib:format("Seems that ~s went offline~s, you'd better check what's up.", [Title, Since]);

format_alarm(Ref, {up, _E}, _WasAlarm) ->
    genlib:format("Seems that ~s went back online, cheers!", [format_ref(Ref)]).

-spec format_ref(deathtoll:cref()) -> binary().

format_ref(Ref) ->
    format_title(genlib:print(Ref, 40)).

format_since(Pre, Extra) ->
    case maps:get(since, Extra, undefined) of
        undefined ->
            <<>>;
        Datetime ->
            Rest = format_datetime(calendar:universal_time_to_local_time(Datetime)),
            <<Pre/bytes, Rest/bytes>>
    end.

format_title(<<L, Rest/bytes>>) when L >= $a, L =< $z ->
    <<(L + ($A - $a)), Rest/bytes>>;

format_title(String) ->
    String.

format_datetime(Datetime) ->
    genlib_format:format_datetime([h, $:, m], Datetime).
