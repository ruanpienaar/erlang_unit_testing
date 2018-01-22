-module(unit_testing).

% Erlang Distribution
-export([
    start_distrib/1,
    start_distrib/2,
    start_distrib/3,
    stop_distrib/0
]).

% Test slave nodes
-export([
    setup_slaves/1,
    cleanup_slaves/1
]).

% Async code testing
-export([
    wait_for_next_value/3
]).

%% @doc This will start the distribution ( shortnames , longnames ) with the supplied Nodename.
%% @end

-spec start_distrib(node()) -> ok.
start_distrib(Name) ->
    do_start_distrib([Name]).

-spec start_distrib(node(), shortnames | longnames) -> ok.
start_distrib(Name, NameType) ->
    do_start_distrib([Name, NameType]).

-spec start_distrib(node(), shortnames | longnames, pos_integer()) -> ok.
start_distrib(Name, NameType, Ticktime) ->
    do_start_distrib([Name, NameType, Ticktime]).

do_start_distrib(A) ->
    case distrib_already_started(node()) of
        true ->
            ok;
        false ->
            [] = os:cmd("epmd -daemon"),
            net_kernel:start(A)
    end.

-spec distrib_already_started(node()) -> boolean().
distrib_already_started('nonode@nohost') ->
    false;
distrib_already_started(_) ->
    true.

-spec stop_distrib() -> ok | {error, not_allowed | not_found}.
stop_distrib() ->
    net_kernel:stop().

-spec setup_slaves(list(tuple())) -> list(node()).
setup_slaves(Slaves) when is_list(Slaves) ->
    lists:map(
        fun({H})       -> slave_node_start(H);
           ({H, N})    -> slave_node_start(H, N);
           ({H, N, A}) -> slave_node_start(H, N, A)
        end, Slaves).

-spec slave_node_start(inet:hostname()) -> node().
slave_node_start(Host) ->
    {ok, SlaveName} = slave:start(Host),
    SlaveName.

-spec slave_node_start(inet:hostname(), atom() | string()) -> node().
slave_node_start(Host, Name) ->
    {ok, SlaveName} = slave:start(Host, Name),
    SlaveName.

-spec slave_node_start(inet:hostname(), atom() | string(), string()) -> node().
slave_node_start(Host, Name, Args) ->
    {ok, SlaveName} = slave:start(Host, Name, Args),
    SlaveName.

-spec cleanup_slaves(list(node())) -> boolean().
cleanup_slaves(Slaves) ->
    lists:all(fun(SlaveNodeName) ->
        case slave:stop(SlaveNodeName) of
            ok ->
                true;
            Reason ->
                error_logger:error_msg(
                    "Could not stop slave ~p Reason ~p",
                    [SlaveNodeName, Reason]
                ),
                false
        end
    end, Slaves).

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