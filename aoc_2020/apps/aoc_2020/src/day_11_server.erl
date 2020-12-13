%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_11_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_11).
-define(NT, neighbour_table).

-record(state, {start_time}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({process_data, Line, AccumulatedData}, _From, S = #state{}) ->
  UpdatedData = process_data(Line, AccumulatedData),
  {reply, {ok, UpdatedData}, S};
handle_call(_Request, _From, S = #state{}) ->
  {reply, ok, S}.

handle_cast({data_ready, part_01, {DataTable, RowNum, ColNum}}, S = #state{}) ->
  io:fwrite("day_11, data_ready... ~p rows x ~p cols~n", [RowNum, ColNum]),
  ets:new(?NT, [ordered_set, named_table]),
  OccupiedCount = get_final_occupied_count(DataTable, RowNum, ColNum, 4),
  TimeDiff = erlang:system_time(millisecond) - S#state.start_time,
  io:fwrite("Occupied count=~p; Execution time=~pms~n", [OccupiedCount, TimeDiff]),
  ets:delete(?NT),
  {noreply, S};
handle_cast({data_ready, part_02, {DataTable, RowNum, ColNum}}, S = #state{}) ->
  io:fwrite("day_11, data_ready... ~p rows x ~p cols~n", [RowNum, ColNum]),
  ets:new(?NT, [ordered_set, named_table]),
  OccupiedCount = get_final_occupied_count(DataTable, RowNum, ColNum, 5, -1),
  TimeDiff = erlang:system_time(millisecond) - S#state.start_time,
  io:fwrite("Occupied count=~p; Execution time=~pms~n", [OccupiedCount, TimeDiff]),
  ets:delete(?NT),
  {noreply, S};
handle_cast(_Request, S = #state{}) ->
  {noreply, S}.

handle_info(part_01, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_11, part_01, {process_data, {ets:new(current, [ordered_set]), 0, 0}}, self()}),
  {noreply, S#state{start_time = erlang:system_time(millisecond)}};
handle_info(part_02, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_11, part_02, {process_data, {ets:new(current, [ordered_set]), 0, 0}}, self()}),
  {noreply, S#state{start_time = erlang:system_time(millisecond)}};
handle_info(_Info, S = #state{}) ->
  {noreply, S}.

terminate(_Reason, _S = #state{}) ->
  ok.

code_change(_OldVsn, S = #state{}, _Extra) ->
  {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_data(_DataTable, [], Row, Col) ->
  {Row + 1, Col};
process_data(DataTable, [Char | Rest], Row, Col) ->
  case Char of
    $L -> ok;
    $# -> ets:insert(DataTable, {{Row, Col}, occupied});
    $. -> ets:insert(DataTable, {{Row, Col}, floor});
    Other -> throw(io_lib:format("Unrecognized symbol: ~p", [Other]))
  end,
  process_data(DataTable, Rest, Row, Col + 1).
process_data(Line, AccumulatedData) ->
  {DataTable, RowNum, ColNum} = AccumulatedData,
  {NewRowNum, NewColumnCount} = case process_data(DataTable, Line, RowNum, 0) of
                                  {Row, Col} when ColNum =:= 0 -> {Row, Col};
                                  {_, Col} when Col =/= ColNum ->
                                    throw("all lines should have the same number of columns");
                                  {Row, Col} -> {Row, Col}
                                end,
  {DataTable, NewRowNum, NewColumnCount}.

calculate_neighbours(Row, Col, {RowCount, ColCount}) ->
  Neighbours = lists:foldl(fun(Y, RAcc) ->
    case Row + Y of
      R when R < 0; R >= RowCount -> RAcc;
      R ->
        CResult = lists:foldl(fun(X, CAcc) ->
          case Col + X of
            C when C < 0; C >= ColCount -> CAcc;
            C when {R, C} =:= {Row, Col} -> CAcc;
            C -> sets:add_element({R, C}, CAcc)
          end
                              end,
          RAcc, [-1, 0, 1]),
        sets:union(RAcc, CResult)
    end
                           end, sets:new(), [-1, 0, 1]),
  sets:to_list(Neighbours).
get_neighbours(Row, Column, Dimensions) ->
  case ets:match(?NT, {Row, Column}) of
    [] ->
      Neighbours = calculate_neighbours(Row, Column, Dimensions),
      ets:insert(?NT, {{Row, Column}, Neighbours}),
      Neighbours;
    [{_Key, Neighbours} | _] -> Neighbours
  end.

count_occupied_adjacent_cells(DataTable, _CurrentCell, NeighbourCells) ->
  lists:foldl(fun(N, Acc) ->
    Acc + case ets:lookup(DataTable, N) of
            [] -> 0;
            [{_, floor}] -> 0;
            [{_, empty}] -> 0;
            [{_, occupied}] -> 1
          end
              end,
    0, NeighbourCells).

make_cell_counting_function(_Range, Limits) ->
  case _Range of
    1 -> fun count_occupied_adjacent_cells/3;
    R when R =:= -1; R >= 2 -> fun(DataTable, CurrentCell, Cells) ->
      {RowLimit, ColLimit} = Limits,
      RangeCheck = fun({Row, Col}) ->
        (0 =< Row) and (Row =< RowLimit) and (0 =< Col) and (Col =< ColLimit)
                   end,
      Seeker = fun Seeker(CC, N, M) ->
        Delta = lists:zipwith(fun(A, B) -> B - A end, tuple_to_list(CC), tuple_to_list(N)),
        NewCell = list_to_tuple(lists:zipwith(fun(A, B) ->
          A + B end, tuple_to_list(CC), lists:map(fun(Coord) -> Coord * M end, Delta))),
        case RangeCheck(NewCell) of
          false ->
            0;
          true -> Result = ets:lookup(DataTable, NewCell),
            case Result of
              [] -> 0;
              [{_, floor}] -> Seeker(CC, N, M + 1);
              [{_, occupied}] -> 1
            end
        end
               end,
      lists:foldl(fun(N, Acc) ->
        Acc + case ets:lookup(DataTable, N) of
                [] -> 0;
                [{_, floor}] -> Seeker(CurrentCell, N, 2);
                [{_, empty}] -> 0;
                [{_, occupied}] -> 1
              end
                  end,
        0, Cells)
                               end;
    R -> throw(io_lib:format("~p is outside of the usable range for cell searches", [R]))
  end.


get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount, CellCountingFunction) ->
  Neighbours = get_neighbours(CurrentRow, CurrentColumn, {RowCount, ColCount}),
  OccupiedNeighbourCount = CellCountingFunction(DataTable, {CurrentRow, CurrentColumn}, Neighbours), %%count_occupied_cells(DataTable, Neighbours),
  OccupiedNeighbourCount.

tick(_DataTable, _TempTable, _Dimensions, [], [], _NeighbourFunction, _MaxOccupiedNeighbours, ChangedCells) ->
  ChangedCells;
tick(DataTable, TempTable, {RowCount, ColCount}, [_CurrentRow | RemainingRows], [], MaxOccupiedNeighbours, CellCountingFunction, ChangedCells) ->
  ColIndices = case RemainingRows of
                 [] -> [];
                 _ -> lists:seq(0, ColCount - 1)
               end,
  tick(DataTable, TempTable, {RowCount, ColCount}, RemainingRows, ColIndices, MaxOccupiedNeighbours, CellCountingFunction, ChangedCells);
tick(DataTable, TempTable, {RowCount, ColCount}, Rows, [CurrentColumn | RemainingColumns], MaxOccupiedNeighbours, CellCountingFunction, ChangedCells) ->
  [CurrentRow | _] = Rows,
  CurrentCell = ets:lookup(DataTable, {CurrentRow, CurrentColumn}),
  UpdatedChangedCells = case CurrentCell of
                          [{_, floor}] ->
                            ets:insert(TempTable, {{CurrentRow, CurrentColumn}, floor}),
                            ChangedCells;
                          [] ->
                            OccupiedNeighbourCount = get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount, CellCountingFunction),
                            if OccupiedNeighbourCount =:= 0 ->
                              ets:insert(TempTable, {{CurrentRow, CurrentColumn}, occupied}),
                              ChangedCells + 1;
                              true -> ChangedCells
                            end;
                          _ ->
                            OccupiedNeighbourCount = get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount, CellCountingFunction),
                            if OccupiedNeighbourCount < MaxOccupiedNeighbours ->
                              ets:insert(TempTable, {{CurrentRow, CurrentColumn}, occupied}),
                              ChangedCells;
                              true -> ChangedCells + 1
                            end
                        end,
  tick(DataTable, TempTable, {RowCount, ColCount}, Rows, RemainingColumns, MaxOccupiedNeighbours, CellCountingFunction, UpdatedChangedCells).

tick(DataTable, [RowCount, ColCount], MaxOccupiedNeighbours, CellCountingFunction) ->
  [RowIndices, ColIndices] = [lists:seq(0, RowCount - 1), lists:seq(0, ColCount - 1)],
  TempTable = ets:new(temp_table, [ordered_set]),
  ChangedCells = tick(DataTable, TempTable, {RowCount, ColCount}, RowIndices, ColIndices, MaxOccupiedNeighbours, CellCountingFunction, 0),
%%  print_current_state(TempTable, RowCount, ColCount),
  case ChangedCells of
    0 ->
      TempTable;
    CC ->
      io:fwrite("ChangedCells: ~p~n", [CC]), % useful progress indicator
      ets:delete(DataTable),
      tick(TempTable, [RowCount, ColCount], MaxOccupiedNeighbours, CellCountingFunction)
  end.

get_final_occupied_count(DataTable, RowCount, ColCount, MaxOccupiedNeighbours) ->
  get_final_occupied_count(DataTable, RowCount, ColCount, MaxOccupiedNeighbours, 1).

get_final_occupied_count(DataTable, RowCount, ColCount, MaxOccupiedNeighbours, SearchRange) ->
  CellCountingFunction = make_cell_counting_function(SearchRange, {RowCount - 1, ColCount - 1}),
%%  print_current_state(DataTable, RowCount, ColCount),
  ResultTable = tick(DataTable, [RowCount, ColCount], MaxOccupiedNeighbours, CellCountingFunction),
  length(ets:match(ResultTable, {'$1', occupied})).

% useful to switch on for dev, but really on when working with a reduced data set
print_current_state(Table, Rows, Cols) ->
  RowIndices = lists:seq(0, Rows - 1),
  io:fwrite("~s~n", [string:pad("", Cols, leading, $-)]),
  lists:foreach(fun(R) ->
    CellValues = lists:map(fun(C) ->
      case ets:match(Table, {{R, C}, '$1'}) of
        [] -> $L;
        [[floor]] -> $.;
        [[occupied]] -> $#
      end
                           end, lists:seq(0, Cols - 1)),
    io:fwrite("~s~n", [CellValues])
                end,
    RowIndices),
  io:fwrite("~s~n", [string:pad("", Cols, leading, $-)]).

% dimensions 0-98, 0-91
% part 01 result: Occupied count=2359; Execution time=8155ms
% part 02 result: Occupied count=2131; Execution time=9292ms
