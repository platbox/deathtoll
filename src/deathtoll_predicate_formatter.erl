%%
%% Alert-to-text formatter which takes into account failed json predicate validations.

-module(deathtoll_predicate_formatter).

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

format_alarm(Ref, {down, E}, _WasAlarm) ->
    Title = format_ref(Ref),
    Since = format_since(<<" at ">>, E),
    Description = format_what(E),
    genlib:format("~s gone bad~s. ~s", [Title, Since, Description]);

format_alarm(Ref, {up, _E}, _WasAlarm) ->
    genlib:format("~s is healthy again, cheers!", [format_ref(Ref)]).

format_ref(Ref) ->
    deathtoll_formatter:format_ref(Ref).

format_since(Pre, Extra) ->
    case maps:get(since, Extra, undefined) of
        undefined ->
            <<>>;
        Datetime ->
            Rest = format_datetime(calendar:universal_time_to_local_time(Datetime)),
            <<Pre/bytes, Rest/bytes>>
    end.

format_what(Extra) ->
    [What, Why, Value] = genlib_map:mget([what, {why, <<"Wrong response">>}, value], Extra),
    iolist_to_binary([
        Why,
        case What of undefined -> <<>>; _ -> [<<" for ">>, What] end,
        case Value of undefined -> <<>>; _ -> [<<", it's ">>, Value] end,
        $.
    ]).

format_datetime(Datetime) ->
    genlib_format:format_datetime([h, $:, m], Datetime).
