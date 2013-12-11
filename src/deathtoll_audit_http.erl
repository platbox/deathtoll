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
    expect  :: ok | {status, 200..399},
    timeout :: pos_integer()
}).

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
    {alarm, case httpc:request(Method, Request, HttpOptions, Options) of
        {ok, _} when Expect =:= ok ->
            {up, []};
        {ok, {StatusCode, _}} when Expect =:= {status, StatusCode} ->
            {up, []};
        {ok, {StatusCode, Body}} ->
            {down, [{status, StatusCode}, {body, Body}]};
        {error, Reason} ->
            {down, [{error, Reason}]}
    end}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.
