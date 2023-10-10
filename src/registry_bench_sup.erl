%%%-------------------------------------------------------------------
%% @doc registry_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(registry_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => bench,
            start => {registry_bench, start_link, [opts()]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

opts() ->
    WithConflict = erlang:list_to_atom(os:getenv("WITH_CONFLICT")),
    AllNodesCount = erlang:list_to_integer(os:getenv("ALL_NODES_COUNT")),
    %% FlapNodesCount = erlang:list_to_integer(os:getenv("FLAP_NODES_COUNT")),
    ProcCount = erlang:list_to_integer(os:getenv("PROC_COUNT")),
    IsCoordinator = is_coordinator(node(), AllNodesCount),
    ReportDir = os:getenv("REPORT_DIR"),
    #{
        is_coordinator => IsCoordinator,
        with_conflict => WithConflict,
        all_nodes_count => AllNodesCount,
        proc_count => ProcCount,
        report_dir => ReportDir
    }.

is_coordinator(Nodename, AllNodesCount) ->
    Name = erlang:atom_to_binary(Nodename),
    [ShortName | _] = binary:split(Name, <<"@">>),
    NodeNum = erlang:binary_to_integer(ShortName),
    NodeNum =:= AllNodesCount.
