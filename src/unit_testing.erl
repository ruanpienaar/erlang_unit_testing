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

% Fixture utilities
-export([
    setup/2,
    setup/3,
    setup/4,
    setup/5,
    foreach/2,
    foreach/3,
    foreach/4,
    foreach/5
]).

% mock ( Using meck )
-export([
    new_mock/2,
    mock_expect/3,
    mock_expect/4
]).

%% @doc mocks :: {module, [options], [expects()]}
%% @end expects() :: {Function, Arity, Response} | {Function, ExpectFun}

% {setup, Setup, Tests | Instantiator}
% {setup, Setup, Cleanup, Tests | Instantiator}
% {setup, Where, Setup, Tests | Instantiator}
% {setup, Where, Setup, Cleanup, Tests | Instantiator}
setup(Setup, Tests) when is_function(Setup, 0) andalso
                         is_list(Tests) ->
    {setup, Setup, Tests};
setup(Setup, {with, Tests}) when is_function(Setup, 0) andalso
                                 is_list(Tests) ->
    {setup, Setup, {with, Tests}};
setup(Setup, Instantiator) when is_function(Setup, 0) andalso
                                is_function(Instantiator) ->
    {with, Setup, Instantiator}.

setup(Setup, Cleanup, Tests) when is_function(Setup, 0) andalso
                                  is_function(Cleanup, 1) andalso
                                  is_list(Tests) ->
    {setup, Setup, Cleanup, Tests}.

setup(Setup, Cleanup, Tests, Mocks) when is_function(Setup, 0) andalso
                                         is_function(Cleanup, 1) andalso
                                         is_list(Tests) andalso
                                         is_list(Mocks) ->
    setup(Setup, Cleanup, Tests, Mocks, false).

setup(Setup, Cleanup, Tests, Mocks, StrickMocks) when is_function(Setup, 0) andalso
                                                            is_function(Cleanup, 1) andalso
                                                            is_list(Tests) andalso
                                                            is_list(Mocks) ->
    %% TODO: -get list of app's and env's 
    %%       -get system state, and restore once test done
    MockMods = lists:sort(proplists:get_keys(Mocks)),
    %?debugFmt("MockMods ~p",[MockMods]),
    {setup,
     fun() ->
        % Apply mocks
        lists:foreach(fun({Mod, ModOpts, Expectations}) when is_atom(Mod) andalso
                                                             is_list(ModOpts) andalso
                                                             is_list(Expectations) ->
            % ok = meck:new(Mod, ModOpts),
            ok = new_mock(Mod, ModOpts),
            lists:foreach(
                fun({Function, Arity, Response}) ->
                    mock_expect(Mod, Function, Arity, Response);
                   ({Function, ExpectFun}) when is_function(ExpectFun) ->
                    mock_expect(Mod, Function, ExpectFun)
                end
            , Expectations)
        end, Mocks),
        Setup()
     end,
     fun(SetupResponse) ->
        % Run cleanup
        Cleanup(SetupResponse),
        % Cleanup mocks
        case StrickMocks of
            true ->
                % ?debugFmt("Strick meck unload", []),
                ?assertEqual(
                    MockMods,
                    lists:sort(meck:unload())
                );
            false ->
                % ?debugFmt("NON Strick meck unload", []),
                ?assert(
                    is_list(meck:unload())
                )
        end,
        erase(), % Delete all the mods from the eunit process.
        % Cleanup links
        {links, Links} = erlang:process_info(self(), links),
        % ?debugFmt("LINKS ~p", [Links]),
        lists:foreach(fun(Pid) ->
            D = erlang:process_info(Pid, [current_function, links]),
            % ?debugFmt("LINK Details ~p", [D]),
            case D of
                [{current_function, {eunit_proc, _, _}}, _] ->
                    ok;
                X ->
                    ?debugFmt(
                        " === REMOVE LINK === ~n~p~n",
                        [X]
                    ),
                    true = erlang:unlink(Pid),
                    true = erlang:exit(Pid, kill)
            end
        end, Links)
     end,
     % List of tests
     Tests
    }.

% TODO:
% {foreach, Setup, [Tests | Instantiator]}
% {foreach, Setup, Cleanup, [Tests | Instantiator]}
% {foreach, Where, Setup, Cleanup, [Tests | Instantiator]}
% {foreach, Where, Setup, [Tests | Instantiator]}

foreach(Setup, Tests) when is_function(Setup, 0),
                           is_list(Tests) ->
    {foreach, Setup, Tests}.

foreach(Setup, Cleanup, Tests) when is_function(Setup, 0),
                                    is_function(Cleanup, 1),
                                    is_list(Tests) ->
    {foreach, Setup, Tests}.

foreach(Setup, Cleanup, Tests, Mocks) ->
    foreach(Setup, Cleanup, Tests, Mocks, false).

%% TODO: simplify this with setup/5
foreach(Setup, Cleanup, Tests, Mocks, StrickMocks) ->
    %% TODO: -get list of app's and env's 
    %%       -get system state, and restore once test done
    MockMods = lists:sort(proplists:get_keys(Mocks)),
    {foreach,
     fun() ->
        % Apply mocks
        lists:foreach(fun({Mod, ModOpts, Expectations}) when is_atom(Mod) andalso
                                                             is_list(ModOpts) andalso
                                                             is_list(Expectations) ->
            % ok = meck:new(Mod, ModOpts),
            ok = new_mock(Mod, ModOpts),
            lists:foreach(
                fun({Function, Arity, Response}) ->
                    mock_expect(Mod, Function, Arity, Response);
                   ({Function, ExpectFun}) when is_function(ExpectFun) ->
                    mock_expect(Mod, Function, ExpectFun)
                end
            , Expectations)
        end, Mocks),
        Setup()
     end,
     fun(SetupResponse) ->
        % Run cleanup
        Cleanup(SetupResponse),
        % Cleanup mocks
        case StrickMocks of
            true ->
                % ?debugFmt("Strick meck unload", []),
                ?assertEqual(
                    MockMods,
                    lists:sort(meck:unload())
                );
            false ->
                % ?debugFmt("NON Strick meck unload", []),
                ?assert(
                    is_list(meck:unload())
                )
        end,
        erase(), % Delete all the mods from the eunit process.
        % Cleanup links
        {links, Links} = erlang:process_info(self(), links),
        % ?debugFmt("LINKS ~p", [Links]),
        lists:foreach(fun(Pid) ->
            D = erlang:process_info(Pid, [current_function, links]),
            % ?debugFmt("LINK Details ~p", [D]),
            case D of
                [{current_function, {eunit_proc, _, _}}, _] ->
                    ok;
                X ->
                    ?debugFmt(
                        " === REMOVE LINK === ~n~p~n",
                        [X]
                    ),
                    true = erlang:unlink(Pid),
                    true = erlang:exit(Pid, kill)
            end
        end, Links)
     end,
     % List of tests
     Tests
    }.

new_mock(Mod, ModOpts) ->
    ok = meck:new(Mod, ModOpts),
    save_mock(Mod, ModOpts),
    ok.

save_mock(Mod, ModOpts) ->
    true = ets_insert({Mod, ModOpts}).

ets_insert(Rec) ->
    case ets:info(?MODULE) of
        undefined ->
            ?MODULE = ets:new(?MODULE, [public, named_table, ordered_set]);
        _ ->
            ok
    end,
    ets:insert(?MODULE, Rec).

mock_expect(Mod, Function, ExpectFun) ->
    case get_mock(Mod) of
        [] ->
            throw({module, Mod, needs_meck_new_first});
        _ ->
            ok = meck:expect(Mod, Function, ExpectFun)
    end.

mock_expect(Mod, Function, Arity, Response) ->
    case get_mock(Mod) of
        [] ->
            throw({module, Mod, needs_meck_new_first});
        _ ->
            ok = meck:expect(Mod, Function, Arity, Response)
    end.

get_mock(Mod) ->
    ets:lookup(?MODULE, Mod).

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