%%
%% Mail alarmist

%% Relies on the `gen_smtp` SMTP library, please do not forget to depend
%% on it if you wish to use this alarmist.
%%
%% https://github.com/Vagabond/gen_smtp.git

-module(deathtoll_alarm_mailer).
-behaviour(deathtoll_alarmist).

-export([
    init/2,
    alarm/3,
    terminate/2
]).

-record(state, {
    to :: [email_address()],
    from :: {email_address(), string()},
    opts :: list()
}).

-type state() :: #state{}.
-type email_address() :: string().

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, Options = #{from := From, to := To, smtp := SmtpOptions}) ->
    Me = maps:get(me, Options, "Deathtoll"),
    _ = is_email(From) orelse error({badarg, From}),
    _ = lists:all(fun is_email/1, To) orelse error({badarg, To}),
    State = #state{
        to = To,
        from = {From, Me},
        opts = SmtpOptions
    },
    {ok, State}.

-spec alarm(deathtoll:cref(), deathtoll:alarm(), state()) -> {ok, state()}.

alarm(Ref, Alarm, State = #state{from = {From, Me}, to = To, opts = Options}) ->
    Plain = deathtoll_plain_formatter:format_alarm(Ref, Alarm),
    Html = deathtoll_html_formatter:format_alarm(Ref, Alarm, #{title => "Deathtoll"}),
    Headers = [
        {<<"Subject">> , iolist_to_binary([deathtoll_format:format_ref(Ref), " is dead"])},
        {<<"From">>    , format_email_list([{Me, From}])},
        {<<"To">>      , format_email_list(To)}
    ],
    Body = mimemail:encode({
        <<"multipart">>, <<"alternative">>,
        Headers,
        [],
        [
            {<<"text">>, <<"plain">>, [], [], Plain},
            {<<"text">>, <<"html">>, [], [], Html}
        ]
    }),
    EMail = {From, To, Body},
    _Pid = gen_smtp_client:send(EMail, Options, fun report/1),
    {ok, State}.

format_email_list(EMails) ->
    smtp_util:combine_rfc822_addresses(lists:map(
        fun (E = {_, _}) -> E; (E) -> {undefined, E} end,
        EMails
    )).

report({ok, Receipt}) ->
    error_logger:info_msg("Mail sent: ~p", [Receipt]);

report(Error) ->
    error_logger:error_msg("Mail sending failed: ~p", [Error]).

-spec terminate(deathtoll:cref(), state()) -> ok.

terminate(_Ref, _State) ->
    ok.

%%

is_email(String) ->
    %% just checks that it looks like an email, nothing special
    nomatch =/= re:run(String, <<"^[\\w+-_.]+@[a-z-_.]+[^.]$">>, [{capture, none}]).
