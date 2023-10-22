-module(registry_bench).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([
    run_after/2,
    report/1
]).

-define(SERVER, ?MODULE).
-define(REPORT_FILE, "report.json").

%%%===================================================================
%%% API
%%%===================================================================

run_after(Timeout, Coordinator) ->
    gen_server:cast(?MODULE, {fire, Timeout, Coordinator}).

report(Report) ->
    gen_server:cast(?MODULE, {report, Report}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Opts) ->
    io:format(user, "start bench with ~p~n", [Opts]),
    gen_server:start_link(
        {local, ?SERVER},
        ?MODULE,
        Opts#{reports => []},
        []
    ).

init(#{is_coordinator := true} = Opts) ->
    {ok, Opts, {continue, coordinate}};
init(Opts) ->
    {ok, Opts}.

handle_continue(coordinate, #{all_nodes_count := AllNodesCount} = State) ->
    timer:sleep(3000),
    ok = assemble_cluster(AllNodesCount),
    ok = countdown(),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({fire, Timeout, Coordinator}, State) ->
    erlang:start_timer(Timeout, self(), fire),
    {noreply, State#{coordinator => Coordinator}};
handle_cast({report, Report}, #{report_dir := ReportDir, reports := Reports, all_nodes_count := AllNodesCount} = State) ->
    io:format(user, "New report: ~p~n", [Report]),
    NewReports = [Report | Reports],
    case erlang:length(NewReports) =:= AllNodesCount of
        true -> do_finish(NewReports, ReportDir);
        false -> wait_reports
    end,
    {noreply, State#{reports => NewReports}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, fire}, #{with_conflict := WithConflict, proc_count := ProcCount,
    coordinator := Coordinator} = State) ->

    IDs = create_ids(ProcCount, WithConflict),
    RegFun = application:get_env(registry_bench, reg_fun, fun global:register_name/2),
    Report = do_test(IDs, RegFun),
    true = rpc:cast(Coordinator, ?MODULE, report, [Report]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_test(IDs, RegFun) ->
    Fun = fun() -> receive _M -> stop end end,
    Flow = fun() ->
        lists:foldl(fun(Id, Acc) ->
            [ RegFun(Id, spawn(Fun)) | Acc ]
        end, [], IDs)
    end,
    {Time, Results} = timer:tc(Flow),
    Rep = lists:foldl(fun
        (yes, #{success := YC} = Acc) -> Acc#{success => YC + 1};
        (no, #{fail := NC} = Acc) -> Acc#{fail => NC + 1}
    end, #{success => 0, fail => 0}, Results),
    Rep#{
        node => node(),
        time => Time,
        total => erlang:length(global:registered_names())
    }.

assemble_cluster(AllNodesCount) ->
    Host = get_host(),
    ok = lists:foreach(fun(Num) ->
        pong = net_adm:ping(make_node_name(Num, Host))
    end, lists:seq(1, AllNodesCount - 1)).

make_node_name(Num, Host) ->
    erlang:binary_to_atom(<<(erlang:integer_to_binary(Num))/binary, "@", Host/binary>>).

get_host() ->
    Name = erlang:atom_to_binary(node()),
    [_, Host] = binary:split(Name, <<"@">>),
    Host.

countdown() ->
    rand:uniform(75),
    Timeout = 5000,
    lists:foreach(fun({Num, Node}) ->
        rpc:cast(Node, ?MODULE, run_after, [Timeout - (Num * (100 + rand:uniform(75))), node()])
    end, lists:enumerate([node() | nodes()])).

create_ids(ProcCount, true) ->
    lists:seq(1, ProcCount);
create_ids(ProcCount, false) ->
    Node = node(),
    lists:foldl(fun(Num, Acc) -> [{Num, Node} | Acc] end, [], lists:seq(1, ProcCount)).

do_finish(Reports, ReportDir) ->
    file:write_file(filename:join([ReportDir, ?REPORT_FILE]), jsx:encode(Reports)),
    lists:foreach(fun(Node) -> rpc:cast(Node, erlang, halt, []) end, nodes()),
    erlang:halt().
