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
    opts :: list(),
    alarm :: undefined | deathtoll:alarm(),
    formatter :: module()
}).

-type state() :: #state{}.
-type email_address() :: string().

-spec init(deathtoll:cref(), deathtoll:options()) -> {ok, state()}.

init(_Ref, Options) ->
    {[From, To, Me, Formatter], SmtpOptions} = deepprops:split([from, to, {me, "Deathtoll"}, {formatter, deathtoll_formatter}], Options),
    _ = is_email(From) orelse error({badarg, From}),
    _ = lists:all(fun is_email/1, To) orelse error({badarg, To}),
    State = #state{
        to = To,
        from = {From, Me},
        opts = SmtpOptions,
        formatter = Formatter
    },
    {ok, State}.

-spec alarm(deathtoll:cref(), deathtoll:alarm(), state()) -> {ok, state()}.

alarm(Ref, Alarm, State = #state{from = {From, Me}, to = To, opts = Options, alarm = WasAlarm, formatter = Formatter}) ->
    Text = Formatter:format_alarm(Ref, Alarm, WasAlarm),
    Body = "Subject: " ++ Formatter:format_ref(Ref) ++ "\r\n"
        "From: " ++ Me ++ "\r\n\r\n" ++ Text,
    EMail = {From, To, Body},
    _Pid = gen_smtp_client:send(EMail, Options, fun report/1),
    {ok, State#state{alarm = Alarm}}.

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
