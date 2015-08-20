%%
%% Web handler for inbox auditor

-module(deathtoll_audit_inbox_handler).
-behaviour(cowboy_http_handler).

-export([routes/0]).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%

routes() ->
    [{"/inbox/:cref/push", ?MODULE, []}].

init(_Transport, Req, _) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {CRef   , Req2} = cowboy_req:binding(cref, Req),
    {Method , Req3} = cowboy_req:method(Req2),
    HasBody         = cowboy_req:has_body(Req3),
    {CType  , Req4} = cowboy_req:header(<<"content-type">>, Req3),
    {ok, Req5} = maybe_accept(Method, HasBody, CType, binary_to_atom(CRef, latin1), Req4),
    {ok, Req5, State}.

maybe_accept(<<"POST">>, true, <<"application/json", _/binary>>, CRef, Req) ->
    case cowboy_req:body(Req) of
        {ok, Body, Req2} ->
            try jiffy:decode(Body, [return_maps]) of
                Object = #{} ->
                    accept(Object, CRef, Req2);
                _ ->
                    fail(400, "Request entity is not a valid json object", Req)
            catch
                _:_ ->
                    fail(400, "Request entity is not a valid json", Req)
            end;
        {more, _, Req2} ->
            fail(413, "Request entity too big", Req2);
        {error, Reason} ->
            fail(400, ["Error while fetching body: ", genlib:print(Reason, 256)], Req)
    end;
maybe_accept(<<"POST">>, true, _CType, _CRef, Req) ->
    fail(406, [{<<"accept">>, ctype()}], "Invalid content type", Req);
maybe_accept(<<"POST">>, false, _CType, _CRef, Req) ->
    cowboy_req:reply(400, "No request entity", Req);
maybe_accept(_, _, _, _, Req) ->
    cowboy_req:reply(405, [{<<"allow">>, <<"POST">>}], "Invalid method", Req).

accept(Object, CRef, Req) ->
    case deathtoll_audit_inbox:push(CRef, genlib_map:atomize(Object)) of
        ok ->
            ok(Req);
        {error, Reason} ->
            fail(400, ["Bad request: ", Reason], Req)
    end.

fail(Code, Description, Req) ->
    fail(Code, [], Description, Req).
fail(Code, Headers0, Description, Req) ->
    _ = error_logger:error_msg(iolist_to_binary(Description)),
    Headers = [{<<"content-type">>, ctype()} | Headers0],
    Body = jiffy:encode(iolist_to_binary(Description)),
    cowboy_req:reply(Code, Headers, Body, Req).

ok(Req) ->
    cowboy_req:reply(204, Req).

ctype() ->
    <<"application/json; charset=utf-8">>.

terminate(_Reason, _Req, _State) ->
    ok.
