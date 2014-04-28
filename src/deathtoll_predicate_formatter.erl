%%
%% Alert-to-text formatter which takes into account failed json predicate validations.

-module(deathtoll_predicate_formatter).

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

format_alarm(Ref, {down, E}, _WasAlarm) ->
    Title = format_ref(Ref),
    Since = format_since(" at ", E),
    Description = format_what(E),
    format("~p gone bad~s. ~s", [Title, Since, Description]);

format_alarm(Ref, {up, _E}, _WasAlarm) ->
    format("~p is healthy again, cheers!", [format_ref(Ref)]).

-spec format_ref(deathtoll:cref()) -> string().

format_ref(Ref) ->
    format_title(format("~p", [Ref])).

format_since(Pre, Extra) ->
    case deepprops:get(since, Extra) of
        undefined -> "";
        Datetime  -> Pre ++ format_datetime(Datetime)
    end.

format_what(Extra) ->
    [What, Why, Value] = deepprops:values([what, {why, <<"Banana">>}, value], Extra),
    iolist_to_binary([
        Why,
        case What of undefined -> []; _ -> [" for ", What] end,
        case Value of undefined -> []; _ -> [", it's ", Value] end,
        "."
    ]).

format_title([L | Rest]) when L >= $a, L =< $z ->
    [L + ($A - $a) | Rest];

format_title(String) ->
    String.

format_datetime({_, {H, M, S}}) ->
    format("~2..0B:~2..0B:~2..0BUTC", [H, M, S]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
