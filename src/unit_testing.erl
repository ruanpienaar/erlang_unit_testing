-module(unit_testing).

-include_lib("eunit/include/eunit.hrl").

% I think Unit tests should not have to deal with Erlang distribution...
% Erlang Distribution
% -export([
%     start_distrib/1,
%     start_distrib/2,
%     start_distrib/3,
%     stop_distrib/0
% ]).

% I think Unit tests should not have to deal with slaves...
% % Test slave nodes
% -export([
%     slaves_setup/1,
%     cleanup_slaves/1
% ]).

% Async code testing
-export([
    wait_for_next_value/3
]).

% I think Unit tests should not have to deal with applications started/stopped
% Running Applications
% -export([
%     stop_extra_applications/0
% ]).

% Eunit test utilities
-export([
    try_test_fun/1
]).

% -spec slaves_setup(list(tuple())) -> list(node()).
% slaves_setup(Slaves) when is_list(Slaves) ->
%     lists:map(
%         fun({H})       -> slave_node_start(H);
%            ({H, N})    -> slave_node_start(H, N);
%            ({H, N, A}) -> slave_node_start(H, N, A)
%         end, Slaves).

% -spec slave_node_start(inet:hostname()) -> node().
% slave_node_start(Host) ->
%     {ok, SlaveName} = slave:start(Host),
%     SlaveName.

% -spec slave_node_start(inet:hostname(), atom() | string()) -> node().
% slave_node_start(Host, Name) ->
%     {ok, SlaveName} = slave:start(Host, Name),
%     SlaveName.

% -spec slave_node_start(inet:hostname(), atom() | string(), string()) -> node().
% slave_node_start(Host, Name, Args) ->
%     {ok, SlaveName} = slave:start(Host, Name, Args),
%     SlaveName.

% -spec cleanup_slaves(list(node())) -> boolean().
% cleanup_slaves(Slaves) ->
%     lists:all(fun(SlaveNodeName) ->
%         case slave:stop(SlaveNodeName) of
%             ok ->
%                 true;
%             Reason ->
%                 error_logger:error_msg(
%                     "Could not stop slave ~p Reason ~p",
%                     [SlaveNodeName, Reason]
%                 ),
%                 false
%         end
%     end, Slaves).

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

% -spec stop_extra_applications() -> list(ok).
% stop_extra_applications() ->
%     [ ok = application:stop(App) ||
%         {App,_ErtsVsn,_Vsn}
%         <- application:which_applications(), App /= kernel andalso App /= stdlib
%     ].

try_test_fun(TestFun) ->
    try
        TestFun(),
        true
    catch
        C:E ->
            ?debugFmt("test failed: ~p",[{C,E,erlang:get_stacktrace()}]),
            false
    end.