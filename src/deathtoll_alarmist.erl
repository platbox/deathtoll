%%
%% Alarmist behaviour

-module(deathtoll_alarmist).

-type state() :: any().

-callback init(deathtoll:cref(), deathtoll:options()) ->
    {ok, state()}.

-callback alarm(deathtoll:cref(), deathtoll:alarm(), state()) ->
    {ok, state()}.

-callback terminate(deathtoll:cref(), state()) ->
    ok.

%%

-export([
    init/3,
    trigger/3,
    terminate/2
]).

-type modstate() :: {module(), state()}.

-spec init(deathtoll:cref(), module(), deathtoll:options()) -> modstate().

init(Ref, Mod, Opts) ->
    {ok, State} = Mod:init(Ref, Opts),
    {Mod, State}.

-spec trigger(deathtoll:cref(), deathtoll:alarm(), modstate()) -> modstate().

trigger(Ref, Alarm, {Mod, State}) ->
    try
        {ok, FinalState} = Mod:alarm(Ref, Alarm, State),
        {Mod, FinalState}
    catch
        C:Error ->
            _ = error_logger:error_msg(
                "~p: Alarm triggering failed with alarm ~p in module ~p with state ~p~n"
                " *** Exception: ~p(~p)~n"
                " *** Stacktrace: ~p~n",
                [Ref, Alarm, Mod, State, C, Error, erlang:get_stacktrace()]
            ),
            {Mod, State}
    end.

-spec terminate(deathtoll:cref(), modstate()) -> any().

terminate(Ref, {Mod, State}) ->
    try Mod:terminate(Ref, State) catch
        C:Error ->
            error_logger:error_msg(
                "~p: Alarm terminating failed in module ~p with state ~p~n"
                " *** Exception: ~p(~p)~n"
                " *** Stacktrace: ~p~n",
                [Ref, Mod, State, C, Error, erlang:get_stacktrace()]
            )
    end.
