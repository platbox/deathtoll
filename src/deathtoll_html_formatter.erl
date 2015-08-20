%%
%% Alarm-to-html formatter.

-module(deathtoll_html_formatter).

-export([format_alarm/3]).

-type template() :: #{
    title => iodata()
}.

-spec format_alarm(deathtoll:cref(), deathtoll:alarm(), template()) ->
    binary().

format_alarm(Ref, Alarm = {State, Extra}, #{title := Title}) ->
    Content = {'body', [
        format_header(State),
        {'article', [
            {'h2', format_heading(State)},
            {'p' , format_manifest(State, Ref)},
            {'p' , format_info(State, Extra)},
            {'h2', "Details"},
            deathtoll_auditor:format(Alarm, {text, html})
        ]}
    ]},
    Inject = #{
        body    => #{'style' => "-ms-text-size-adjust: 100%; -webkit-text-size-adjust: 100%; padding: 0; margin: 0; font-family: sans-serif;"},
        article => #{'style' => "margin: 2em; color: #263238; line-height: 1.6em;"},
        h2      => #{'style' => "margin-top: 1.6em;"},
        pre     => #{'style' => "background-color: #ECEFF1; padding: 1em; border-left: solid 0.5em #78909C; white-space: pre-wrap;"}
    },
    render(Title, Content, #{inject_attributes => Inject}).

format_header(State) ->
    {Color, ColorBorder, Text} = case State of
        down -> {"#FF1744", "#D50000", "ಢ︵ಢ"};
        up   -> {"#00E676", "#00C853", "◕‿◕"}
    end,
    {'div', #{'style' => ["background-color: ", Color, "; padding: 0.6em 2em 0.2em; height: 3.2em; border-bottom: solid 0.4em ", ColorBorder, ";"]}, [
        {'h1', #{'style' => "color: white; margin: 0; font-weight: normal;"}, Text}
    ]}.

format_heading(down) ->
    "Evil tidings";
format_heading(up) ->
    "Good news everyone".

format_manifest(down, Ref) ->
    ["The ", {strong, deathtoll_format:format_ref(Ref)}, " is dead. Where is your God now?"];
format_manifest(up, Ref) ->
    ["The ", {strong, deathtoll_format:format_ref(Ref)}, " is alive. Breathe easy."].

format_info(down, Extra = #{since := Since}) ->
    [
        "Something horribly wrong happened around ", {strong, format_since(Since)},
        format_next(maps:get(n, Extra, 1)), " ",
        format_check(maps:get(seq, Extra, undefined)), " ",
        "There is nothing we could do. Please, take some time to mourn the loss of him and then get back to your pitiful business."
    ];
format_info(up, #{since := Since}) ->
    [
        "Something happened around ", {strong, format_since(Since)}, " and suddenly everything got back to normal."
    ].

format_since(Datetime) ->
    format_datetime(calendar:universal_time_to_local_time(Datetime)).

format_datetime(Datetime) ->
    genlib_format:format_datetime([h, $:, m, $:, s], Datetime).

format_next(1) ->
    ".";
format_next(_) ->
    " and kept being wrong since.".

format_check(undefined) ->
    [];
format_check(1) ->
    [];
format_check(N) ->
    ["We checked ", {strong, N}, " times in a row, it's still wrong."].

%%

render(Title, Content, Options) ->
    unicode:characters_to_binary([
        "<!DOCTYPE html>",
        render(
            {html, [
                {head, [
                    {meta, #{'http-equiv' => "Content-Type", 'content' => "text/html; charset=UTF-8"}},
                    {title, Title}
                ]},
                Content
            ]
        }, Options)
    ]).

render({Name}, Options) when is_atom(Name) ->
    start_tag(Name, [], Options);
render({Name, Attrs = #{}, Content}, Options) ->
    [start_tag(Name, Attrs, Options), render(Content, Options), end_tag(Name)];
render({Name, Attrs = #{}}, Options) when is_atom(Name) ->
    start_tag(Name, Attrs, Options);
render({Name, Content}, Options) when is_atom(Name) ->
    render({Name, #{}, Content}, Options);
render(Input = [C | _], Options) when is_integer(C) ->
    render(unicode:characters_to_binary(Input), Options);
render(Input, Options) when is_list(Input) ->
    [render(Term, Options) || Term <- Input];
render(Input, _Options) when is_integer(Input) ->
    integer_to_list(Input);
render(Input, _Options) when is_binary(Input) ->
    escape(Input);
render(Input, Options = #{context := Context}) when is_atom(Input) ->
    case maps:get(Input, Context, undefined) of
        Term when Term /= undefined ->
            render(Term, Options);
        undefined ->
            atom_to_list(Input)
    end.

escape(Input) ->
    escape(Input, escapes()).
escape(Input, Escapes) ->
    lists:foldl(fun ({P, R}, B) -> binary:replace(B, P, R, [global]) end, Input, Escapes).

start_tag(Name, Attrs, Options) ->
    Inject = maps:get(Name, maps:get(inject_attributes, Options, #{}), #{}),
    [$<, atom_to_list(Name), maps:fold(fun attr_render/3, [], inject_attrs(Attrs, Inject)), $>].

inject_attrs(Attrs, Injected) ->
    maps:merge(Attrs, Injected).

attr_render(Name, undefined, Acc) when is_atom(Name) ->
    [$\s, atom_to_list(Name) | Acc];
attr_render(Name, Value, Acc) when is_atom(Name) ->
    [$\s, atom_to_list(Name), $=, $", attr_value_render(Value), $" | Acc].

attr_value_render(Value) when is_list(Value) ->
    attr_value_render(unicode:characters_to_binary(Value));
attr_value_render(Value) when is_binary(Value) ->
    escape(Value, [{<<"\"">>, <<"&quot;">>} | escapes()]);
attr_value_render(Value) when is_integer(Value) ->
    integer_to_list(Value);
attr_value_render(Value) when is_atom(Value) ->
    atom_to_list(Value).

end_tag(Name) ->
    [$<, $/, atom_to_list(Name), $>].

escapes() ->
    [{<<"&">>, <<"&amp;">>}, {<<"<">>, <<"&lt;">>}, {<<">">>, <<"&gt;">>}].
