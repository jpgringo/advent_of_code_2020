%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_10_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_10).
-define(TAB, day_10_data).

-record(state, {start_time}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({process_data, Line, AccumulatedData}, _From, S = #state{}) ->
%%  io:fwrite("Unknown call request: ~p~n", [_sRequest]),
  UpdatedData = process_data(Line, AccumulatedData),
  {reply, {ok, UpdatedData}, S};
handle_call(_Request, _From, S = #state{}) ->
  io:fwrite("Unknown call request: ~p~n", [_Request]),
  {reply, ok, S}.

handle_cast({data_ready, part_01, ParsedData}, S = #state{}) ->
  io:fwrite("day_10, data_ready... ~p entries~n", [length(ParsedData)]),
  Product = get_diff_product(ParsedData, 1, 3),
  io:fwrite("The product of number of diffs of value 1 and diffs of value 3 = ~p~n", [Product]),
  {noreply, S};
handle_cast({data_ready, part_02, ParsedData}, S = #state{}) ->
  io:fwrite("day_10, data_ready... ~p entries~n", [length(ParsedData)]),
  ets:new(?TAB, [ordered_set, named_table]),
  PathCount = count_all_possible_paths(ParsedData, {1, 3}),
  TimeDiff = erlang:system_time(millisecond) - S#state.start_time,
  io:fwrite("Total path count = ~p; Execution time=~pms~n", [PathCount, TimeDiff]),
  ets:delete(?TAB),
  {noreply, S};
handle_cast(_Request, S = #state{}) ->
  {noreply, S}.

handle_info(part_01, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_10, part_01, {process_data, []}, self()}),
  {noreply, S};
handle_info(part_02, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_10, part_02, {process_data, []}, self()}),
  {noreply, S#state{start_time = erlang:system_time(millisecond)}};
handle_info(_Info, S = #state{}) ->
  {noreply, S}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, S = #state{}, _Extra) ->
  {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_data(Line, AccumulatedData) ->
  case string:to_integer(Line) of
    {error, Msg} -> throw(Msg);
    {Int, _} -> [Int | AccumulatedData]
  end.

get_diff_tallies(JoltageList, Differences) when length(JoltageList) =:= 1 ->
  Differences;
get_diff_tallies([First | Rest], Differences) ->
  [Next | _] = Rest,
  Diff = Next - First,
  UpdatedDifferences = case lists:keyfind(Diff, 1, Differences) of
                         false -> [{Diff, 1} | Differences];
                         {Key, Tally} ->
                           lists:keyreplace(Key, 1, Differences, {Key, Tally + 1})
                       end,
  get_diff_tallies(Rest, UpdatedDifferences).
get_diff_tallies(JoltageList) ->
  OutletJoltage = 0,
  [MaxAdapterJoltage | _] = lists:reverse(lists:sort(JoltageList)),
  DeviceVoltage = MaxAdapterJoltage + 3,
  get_diff_tallies(lists:sort([DeviceVoltage | [OutletJoltage | JoltageList]]), []).

get_diff_product(JoltageList, Diff1, Diff2) ->
  DiffTallies = get_diff_tallies(JoltageList),
  {_, Tally1} = lists:keyfind(Diff1, 1, DiffTallies),
  {_, Tally2} = lists:keyfind(Diff2, 1, DiffTallies),
  Tally1 * Tally2.

get_paths([Vertex | Remaining], {MinDiff, MaxDiff}) when length(Remaining) =:= 1 ->
  [DeviceVertex] = Remaining,
  PathCount = case DeviceVertex - Vertex of
                Diff when MinDiff =< Diff, Diff =< MaxDiff -> 1;
                _ -> 0
              end,
  PathCount;
get_paths([Vertex | Remaining], {MinDiff, MaxDiff}) ->
  CandidateSets = get_candidate_sets(Vertex, Remaining, [], {MinDiff, MaxDiff}),
  lists:foldr(fun(CandidateSet, Acc) ->
    [Candidate | _] = CandidateSet,
    PathCount = case ets:lookup(?TAB, Candidate) of
                  [] ->
                    SubPathCount = get_paths(CandidateSet, {MinDiff, MaxDiff}),
                    ets:insert(?TAB, {Candidate, SubPathCount}),
                    SubPathCount;
                  [{_V, Tally} | _] -> Tally
                end,
    Acc + PathCount
              end,
    0, CandidateSets).

get_candidate_sets(StartNode, RemainingVertices, Candidates, {MinDiff, MaxDiff}) ->
  [Next | Rest] = RemainingVertices,
  case Next - StartNode of
    Diff when MinDiff =< Diff, Diff =< MaxDiff ->
      get_candidate_sets(StartNode, Rest, [RemainingVertices | Candidates], {MinDiff, MaxDiff});
    _ -> Candidates
  end.

count_all_possible_paths(JoltageList, DiffRange) ->
  SortedList = lists:sort(JoltageList),
  get_paths([0 | lists:append(SortedList, [lists:last(SortedList) + 3])], DiffRange).
