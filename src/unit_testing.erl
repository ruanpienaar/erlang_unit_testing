-module(unit_testing).

-include_lib("eunit/include/eunit.hrl").

% Async code testing
-export([
    wait_for_match/3,
    wait_for_match/4,
    wait_for_next_value/3
]).

% Eunit test utilities
-export([
    try_test_fun/1
]).

wait_for_match(Times, F, Match) ->
    wait_for_match(Times, F, Match, 25).

wait_for_match(0, _, _, _) ->
    {error, no_answer};
wait_for_match(Times, F, Match, Sleep) ->
    case F() of
        A when A == Match ->
            ok;
        _ ->
            timer:sleep(Sleep),
            wait_for_match(Times-1, F, Match, Sleep)
    end.

wait_for_next_value(0, _, _) ->
    {error, no_answer};
wait_for_next_value(Times, F, Expected) ->
    case (NextValue = F()) == Expected of
        true ->
            timer:sleep(25),
            % ?debugFmt("..... KEEP ~p CALLING ~p ......~n", [Times, nodes()]),
            wait_for_next_value(Times-1, F, Expected);
        false ->
            NextValue
    end.

    %trace([
    %   {goanna_db}
    %]),

    %trace([
    %   {goanna_db,
    %       [
    %           lookup,
    %           store,
    %           delete_child_id_tracelist,
    %           delete_child_id_trace_pattern
    %       ]
    %   }
    %]),
% trace(L) ->
%     start_dbg(),
%     do_trace(L).

% do_trace(L) when is_list(L) ->
%     [ do_trace(I) || I <- L ];
% do_trace(M) when is_atom(M) ->
%     dbg:tpl(M, cx);
% do_trace({M, Functions}) when is_list(Functions) ->
%     [ do_trace({M, F}) || F <- Functions ];
% do_trace({M, Function}) when is_atom(Function) ->
%     dbg:tpl(M, Function, cx).

% start_dbg() ->
%     dbg:tracer(),
%     dbg:p(all, call).

try_test_fun(TestFun) ->
    try
        TestFun(),
        true
    catch
        C:E ->
            ?debugFmt("test failed: ~p",[{C,E,erlang:get_stacktrace()}]),
            false
    end.