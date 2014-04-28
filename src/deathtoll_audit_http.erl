%%
%% Simple HTTP auditor.

-module(deathtoll_audit_http).
-behaviour(deathtoll_auditor).

-export([
    init/2,
    start/2,
    terminate/2
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
-type jsonobject() :: {[{jsonkey(), jsonvalue()}]}.

-type predicate() :: jsonvalue() | {contains, iodata()} | {compares, [{comparer(), jsonvalue()}]}.
-type comparer() :: '<' | '>' | '>=' | '=<' | '==' | '/='.

-type state() :: #state{}.

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, Options) ->
    ok = deathtoll_app:start_app(ssl),
    ok = deathtoll_app:start_app(inets),
    {ok, #state{
        url = deepprops:require(url, Options),
        body = deepprops:get(body, Options, <<>>),
        ctype = deepprops:get(content_type, Options, "text/plain"),
        expect = deepprops:get(expect, Options, {status, 200}),
        timeout = deepprops:get(timeout, Options, 10000)
    }}.

-spec start(deathtoll:cref(), state()) -> {alarm, deathtoll:alarm()}.

start(_Ref, #state{url = Url, body = Body, ctype = CType, expect = Expect, timeout = Timeout}) ->
    Headers = [
        {"Accept", "*/*"}
    ],
    HttpOptions = [
        {timeout, Timeout},
        {connect_timeout, Timeout},
        {relaxed, true}
    ],
    Options = [
        {body_format, binary},
        {full_result, false}
    ],
    {Method, Request} = if
        Body =:= <<>>; Body =:= "" ->
            {get, {Url, Headers}};
        true ->
            {post, {Url, Headers, CType, Body}}
    end,
    {alarm, expected(httpc:request(Method, Request, HttpOptions, Options), Expect)}.

expected({ok, _}, ok) ->
    {up, []};

expected({ok, {StatusCode, _}}, {status, StatusCode}) ->
    {up, []};

expected({ok, {StatusCode, _}}, {status, StatusCode}) ->
    {up, []};

expected({ok, {StatusCode, Body}}, {response, StatusCode, BodyMatch}) ->
    case binary:match(Body, iolist_to_binary([BodyMatch])) of
        nomatch ->
            {down, [{status, StatusCode}, {body, Body}]};
        _ ->
            {up, []}
    end;

expected({ok, {StatusCode, Body}}, {json, StatusCode, Title, JsonPath, Pred}) ->
    try
        Json = jiffy:decode(Body),
        Value = get_json_value(JsonPath, Json),
        _ = run_predicate(Pred, Value),
        {up, []}
    catch
        throw:{error, {_, _}} ->
            {down, [{what, Title}, {why, <<"No json">>}]};
        throw:badpath ->
            {down, [{what, Title}, {why, <<"No json value at path">>}]};
        throw:{failed, Why, ActualValue} ->
            {down, [{what, Title}, {why, Why}, {value, ActualValue}]}
    end;

expected({ok, {StatusCode, Body}}, _) ->
    {down, [{status, StatusCode}, {body, Body}]};

expected(Error = {error, _}, _) ->
    {down, [Error]}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.

%%

get_json_value([], Value) ->
    Value;

get_json_value([Key | Rest], {Props}) when is_list(Props) ->
    case lists:keyfind(Key, 1, Props) of
        {_, Value} ->
            get_json_value(Rest, Value);
        false ->
            throw(badpath)
    end;

get_json_value(_Path, _Value) ->
    throw(badpath).

run_predicate({contains, Subject}, Value) ->
    Result = binary:match(Value, iolist_to_binary([Subject])),
    run_predicate_result(Result =/= nomatch, <<"No match">>, Value);

run_predicate({compares, Comparers}, Value) ->
    Result = lists:all(fun (C) -> compare(C, Value) end, Comparers),
    run_predicate_result(Result, <<"Conditions not hold">>, Value);

run_predicate(Expect, Value) ->
    run_predicate_result(Expect =:= Value, <<"Mismatch">>, Value).

run_predicate_result(true, _Desc, _Value) ->
    ok;

run_predicate_result(false, Desc, Value) ->
    throw({failed, Desc, to_binary(Value)}).

compare({Comparer, Bound}, Value) ->
    try erlang:Comparer(Value, Bound) catch
        _:_ -> throw({failed, <<"Invalid predicate">>, Value})
    end.

to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V).
