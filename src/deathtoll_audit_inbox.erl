%%
%% Inbox auditor

-module(deathtoll_audit_inbox).
-behaviour(deathtoll_auditor).

-export([push/2]).

-export([
    init/2,
    start/2,
    terminate/2,
    format/2
]).

%%

-spec push(deathtoll:cref(), Message :: #{}) -> ok | {error, string()}.

push(Ref, Message) when is_atom(Ref) ->
    try
        ok = validate(Message),
        Inbox = get_inbox_name(Ref),
        Limit = ets:lookup_element(Inbox, limit, 2),
        case ets:update_counter(Inbox, size, 1) of
            N when N > Limit ->
                ok;
            N ->
                true = ets:insert_new(Inbox, {{message, N}, Message}), ok
        end
    catch
        error:badobject ->
            {error, "invalid message"};
        error:badarg ->
            {error, "invalid inbox"}
    end.

validate(#{message := Text, timestamp := Ts}) when is_binary(Text), is_binary(Ts) ->
    ok;
validate(_) ->
    error(badobject).

%%

-record(state, {table :: atom()}).

-type state() :: #state{}.

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(Ref, Options) ->
    Inbox = get_inbox_name(Ref),
    Limit = maps:get(maxbatch, Options, 20),
    _ = ets:new(Inbox, [public, named_table, {write_concurrency, true}]),
    true = ets:insert_new(Inbox, [{size, 0}, {limit, Limit}]),
    {ok, #state{table = Inbox}}.

-spec start(deathtoll:cref(), state()) -> {alarm, deathtoll:alarm()}.

start(_Ref, #state{table = Inbox}) ->
    case pull(Inbox) of
        {0, _} ->
            {alarm, {up, #{}}};
        {Total, Messages} ->
            {alarm, {down, #{total => Total, messages => Messages}}}
    end.

pull(Inbox) ->
    Total = ets:lookup_element(Inbox, size, 2),
    Messages = ets:match(Inbox, {{message, '_'}, '$1'}),
    true = ets:match_delete(Inbox, {{message, '_'}, '_'}),
    true = ets:insert(Inbox, {size, 0}),
    {Total, [M || [M] <- Messages]}.

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.

-spec format(deathtoll:alarm(), deathtoll_auditor:ctype()) -> term().

format({up, _}, {text, plain}) ->
    <<"No incoming messages.">>;

format({up, _}, {text, html}) ->
    {p, <<"No incoming messages.">>};

format({down, #{total := 1, messages := [#{message := Text}]}}, {text, plain}) ->
    <<"Got message: ", Text/binary>>;
format({down, #{total := Total, messages := [#{message := Text} | _]}}, {text, plain}) ->
    deathtoll_format:render_template(
        <<"Got {{total}} messages, first one of them: {{&text}}">>,
        #{total => Total, text => Text}
    );

format({down, #{total := Total, messages := Messages}}, {text, html}) ->
    [
        {pre, lists:map(fun format_message/1, Messages)} |
            case length(Messages) of
                Total -> [];
                Count -> [{p, ["... and ", {strong, Total - Count}, " other messages."]}]
            end
    ];

format({State, Opts}, {application, json}) ->
    Opts#{state => State}.

format_message(_Message = #{message := Text, timestamp := Ts}) ->
    [{strong, Ts}, " ", {span, Text}, "\n"].

%%

get_inbox_name(Ref) ->
    % TODO
    list_to_atom("$" ++ atom_to_list(Ref) ++ "_inbox").
