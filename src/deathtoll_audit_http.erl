%%
%% Simple HTTP auditor.

-module(deathtoll_audit_http).
-behaviour(deathtoll_auditor).

-export([
    init/2,
    start/2,
    terminate/2,
    format/2
]).

%%

-record(state, {
    url     :: string(),
    body    :: iodata(),
    ctype   :: string(),
    expect  :: expectance(),
    timeout :: pos_integer()
}).

-type expectance() ::
    ok |
    {status, 200..399} |
    {response, 200..399, iodata()} |
    {json, 200..399, binary(), [jsonkey()], predicate()}.

-type jsonkey() :: binary().
-type jsonvalue() :: null | binary() | boolean() | number() | [jsonvalue()] | jsonobject().
-type jsonobject() :: #{jsonkey() => jsonvalue()}.

-type predicate() :: jsonvalue() | {contains, iodata()} | {compares, [{comparer(), jsonvalue()}]}.
-type comparer() :: '<' | '>' | '>=' | '=<' | '==' | '/='.

-type state() :: #state{}.

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, Options) ->
    _ = genlib_app:start_application(hackney),
    {ok, #state{
        url = maps:get(url, Options),
        body = maps:get(body, Options, <<>>),
        ctype = maps:get(content_type, Options, undefined),
        expect = maps:get(expect, Options, {status, 200}),
        timeout = maps:get(timeout, Options, 10000)
    }}.

-spec start(deathtoll:cref(), state()) -> {alarm, deathtoll:alarm()}.

start(_Ref, #state{url = Url, body = Body, ctype = CType, expect = Expect, timeout = Timeout}) ->
    Headers0 = [{<<"accept">>, <<"*/*">>}],
    Headers1 = case CType of
        undefined ->
            Headers0;
        _ ->
            [{<<"content-type">>, CType} | Headers0]
    end,
    Options = [
        {timeout, Timeout},
        {connect_timeout, Timeout}
    ],
    {State, Opts} = if
        Body =:= <<>>; Body =:= "" ->
            assert(hackney:get(Url, Headers0, <<>>, Options), Expect);
        true ->
            assert(hackney:post(Url, Headers1, Body, Options), Expect)
    end,
    {alarm, {State, Opts#{url => Url}}}.

assert({ok, _, _, _}, ok) ->
    {up, #{}};

assert({ok, StatusCode, _, _}, {status, StatusCode}) ->
    {up, #{status => StatusCode}};

assert({ok, StatusCode, _, CRef}, {response, StatusCode, BodyMatch}) ->
    expect_body(CRef, fun (Body) ->
        case binary:match(Body, iolist_to_binary(BodyMatch)) of
            nomatch ->
                {down, #{status => StatusCode, body => Body}};
            _ ->
                {up, #{status => StatusCode, body => Body}}
        end
    end);

assert(Result, {json, StatusCode, JsonPath, Pred}) ->
    Parsed = {Fragments} = parse_json_path(JsonPath),
    Title = deathtoll_format:format_ref(lists:last(Fragments)),
    assert(Result, {json, StatusCode, Title, Parsed, Pred});

assert({ok, StatusCode, _, CRef}, {json, StatusCode, Title, JsonPath, Pred}) ->
    expect_body(CRef, fun (Body) ->
        try
            Json = jiffy:decode(Body, [return_maps]),
            Value = get_json_value(parse_json_path(JsonPath), Json),
            _ = run_predicate(Pred, Value),
            {up, #{what => Title, value => Value}}
        catch
            throw:{error, {_, _}} ->
                {down, #{what => Title, why => <<"No json">>}};
            throw:badpath ->
                {down, #{what => Title, why => <<"No json value at path">>}};
            throw:{failed, Why, ActualValue} ->
                {down, #{what => Title, why => Why, value => ActualValue}}
        end
    end);

assert({ok, StatusCode, _, _}, _) ->
    {down, #{status => StatusCode}};

assert({error, Reason}, _) ->
    {down, #{error => Reason}}.

expect_body(CRef, F) ->
    case hackney:body(CRef) of
        {ok, Body} ->
            F(Body);
        {error, Reason} ->
            {down, #{error => Reason}}
    end.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.

%%

-spec format(deathtoll:alarm(), deathtoll_auditor:ctype()) -> term().

format(Alarm, {text, plain}) ->
    format_plain(Alarm);

format(Alarm, {text, html}) ->
    format_html(Alarm);

format(Alarm, {application, json}) ->
    deathtoll_format:alarm_to_json(Alarm);

format(Alarm, _CType) ->
    format(Alarm, {text, plain}).

format_plain({down, _Opts = #{url := Url, error := Reason}}) ->
    Tpl = <<"Failed to access {{&url}}: {{&reason}}.">>,
    deathtoll_format:render_template(Tpl, #{url => Url, reason => genlib:print(Reason, 120)});

format_plain({down, Opts = #{what := _, why := _}}) ->
    Tpl = <<"Bad value for {{&what}}. {{&why}}{{#value}}, it's {{&value}}{{/value}}.">>,
    deathtoll_format:render_template(Tpl, Opts);

format_plain({up, Opts = #{what := _}}) ->
    Tpl = "Good value for {{&what}}{{#value}}, it's {{&value}}{{/value}}.",
    deathtoll_format:render_template(Tpl, Opts);

format_plain({_, Opts = #{}}) ->
    Tpl = <<"Got response from {{&url}}{{#status}} with status {{&status}}{{/status}}{{#body}}: {{&body}}{{/body}}.">>,
    deathtoll_format:render_template(Tpl, Opts).

format_html({down, _Opts = #{url := Url, error := Reason}}) ->
    [
        {p, ["Failed to request web resource at ", {a, #{href => Url}, Url}, ", the reason is:"]},
        {pre, genlib:print(Reason, 4096)}
    ];

format_html({State, Opts = #{url := Url, what := What}}) ->
    Which = case State of
        up   -> "good";
        down -> "bad"
    end,
    {Prefix, Value} = case maps:get(value, Opts, undefined) of
        V when V /= undefined -> {", current value is:", [{pre, V}]};
        undefined             -> {".", []}
    end,
    Why = case maps:get(why, Opts, undefined) of
        W when W /= undefined -> [". ", W];
        undefined             -> []
    end,
    [
        {p, [
            "Got a ", Which, " value for ", {strong, What},
            " while requesting resource at ", {a, #{href => Url}, Url}, Why, Prefix
        ]} | Value
    ];

format_html({_, Opts = #{url := Url}}) ->
    Status = case maps:get(status, Opts, undefined) of
        S when S /= undefined -> ["with status ", {strong, S}];
        undefined             -> ["successfully"]
    end,
    {Prefix, Body} = case maps:get(body, Opts, undefined) of
        B when B /= undefined -> {":", [{pre, B}]};
        undefined             -> {".", []}
    end,
    [{p, ["Web resource at ", {a, #{href => Url}, Url}, " responded ", Status, Prefix]} | Body].

%%

parse_json_path(Path = {_}) ->
    Path;

parse_json_path(Path) ->
    {binary:split(iolist_to_binary(Path), <<$.>>, [global])}.

get_json_value({[]}, Value) ->
    Value;

get_json_value({[Key | Rest]}, Props = #{}) ->
    case maps:get(Key, Props, Ref = make_ref()) of
        Value when Value =/= Ref ->
            get_json_value({Rest}, Value);
        Ref ->
            throw(badpath)
    end;

get_json_value(_Path, _Value) ->
    throw(badpath).

run_predicate({contains, Subject}, Value) ->
    Result = binary:match(Value, iolist_to_binary(Subject)),
    run_predicate_result(Result =/= nomatch, <<"No match">>, Value);

run_predicate({compares, Comparers}, Value) ->
    Result = lists:all(fun (C) -> compare(C, Value) end, Comparers),
    run_predicate_result(Result, <<"Conditions not hold">>, Value);

run_predicate(Expect, Value) ->
    run_predicate_result(Expect =:= Value, <<"Mismatch">>, Value).

run_predicate_result(true, _Desc, _Value) ->
    ok;

run_predicate_result(false, Desc, Value) ->
    throw({failed, Desc, Value}).

compare({Comparer, Bound}, Value) ->
    try erlang:Comparer(Value, Bound) catch
        _:_ -> throw({failed, <<"Invalid predicate">>, Value})
    end.
