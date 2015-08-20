%%
%% Formatting utilities

-module(deathtoll_format).

%%

-export([format_ref/1]).
-export([format_title/1]).

-export([render_template/2]).
-export([alarm_to_json/1]).

%%

-spec format_ref(deathtoll:cref()) -> binary().

format_ref(Ref) ->
    format_title(genlib:print(Ref, 40)).

-spec format_title(binary()) -> binary().

format_title(<<L, Rest/bytes>>) when L >= $a, L =< $z ->
    <<(L + ($A - $a)), Rest/bytes>>;

format_title(String) ->
    String.

%%

-spec render_template(iodata(), #{}) -> iodata().

render_template(Tpl, Context) ->
    bbmustache:render(genlib:to_binary(Tpl), genlib_map:compact(Context), [{key_type, atom}]).

-spec alarm_to_json(deathtoll:alarm()) -> jiffy:json_value().

alarm_to_json({State, Opts}) ->
    maps:map(
        fun (_, V) -> genlib:print(V, 256) end,
        Opts#{state => State}
    ).
