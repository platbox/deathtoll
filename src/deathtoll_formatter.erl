%%
%% Default alert-to-text formatter

-module(deathtoll_formatter).

-export([
    format_alarm/2,
    format_alarm/3,
    format_ref/1
]).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm()) ->
    string().

format_alarm(Ref, Alarm) ->
    format_alarm(Ref, Alarm, undefined).

-spec format_alarm(deathtoll:cref(), deathtoll:alarm(), WasAlarm :: undefined | deathtoll:alarm()) ->
    string().

format_alarm(Ref, {down, E}, {down, _}) ->
    Title = format_ref(Ref),
    Since = format_since(" since ", E),
    format("Seems that ~p still offline~s, you'd better check what's up.", [Title, Since]);

format_alarm(Ref, {down, E}, _WasAlarm) ->
    Title = format_ref(Ref),
    Since = format_since(" at ", E),
    format("Seems that ~p went offline~s, you'd better check what's up.", [Title, Since]);

format_alarm(Ref, {up, _E}, _WasAlarm) ->
    format("Seems that ~p went back online, cheers!", [format_ref(Ref)]).

-spec format_ref(deathtoll:cref()) -> string().

format_ref(Ref) ->
    format_title(format("~p", [Ref])).

format_since(Pre, Extra) ->
    case deepprops:get(since, Extra) of
        undefined -> "";
        Datetime  -> Pre ++ format_datetime(Datetime)
    end.

format_title([L | Rest]) when L >= $a, L =< $z ->
    [L + ($A - $a) | Rest];

format_title(String) ->
    String.

format_datetime({{_, Mo, D}, {H, Mi, _}}) ->
    format("~B.~2..0B ~2..0B:~2..0B UTC", [D, Mo, H, Mi]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
