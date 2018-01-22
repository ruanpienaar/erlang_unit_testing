-module(unit_testing).

-export([
    start_distrib/2,
    setup_slaves/1,
    cleanup_slaves/1
]).

%% @doc This will start the distribution ( shortnames , longnames ) with the supplied Nodename.
%% @end
-spec start_distrib( node(), shortnames | longnames)
        -> {ok, ActualNodeName::atom} | {error, Reason::term()}.
start_distrib(NodeName, NodeType) when is_atom(NodeName) ->
    case node() of
        'nonode@nohost' ->
            [] = os:cmd("epmd -daemon"),
            case net_kernel:start([NodeName, NodeType]) of
                {ok, _Pid} ->
                    {ok, node()};
                {error, Reason} ->
                    {error, Reason}
            end;
        CurrNode ->
            CurrNode
    end.

-spec setup_slaves(list(tuple())) -> list(node()).
setup_slaves(Slaves) when is_list(Slaves) ->
    lists:map(
        fun({H})       -> slave_node_start(H);
           ({H, N})    -> slave_node_start(H, N);
           ({H, N, A}) -> slave_node_start(H, N, A)
        end, Slaves).

-spec cleanup_slaves(list(node())) -> boolean().
cleanup_slaves(Slaves) ->
    % [ ok = slave:stop(SlaveNodeName) || SlaveNodeName <- Slaves ].
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

