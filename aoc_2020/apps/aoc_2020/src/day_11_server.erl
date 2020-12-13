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
%%  io:fwrite("Unknown call request: ~p~n", [_sRequest]),
  UpdatedData = process_data(Line, AccumulatedData),
  {reply, {ok, UpdatedData}, S};
handle_call(_Request, _From, S = #state{}) ->
  {reply, ok, S}.

handle_cast({data_ready, part_01, {DataTable, RowNum, ColNum}}, S = #state{}) ->
  io:fwrite("day_10, data_ready... ~p rows x ~p cols~n", [RowNum, ColNum]),
  ets:new(?NT, [ordered_set, named_table]),
  OccupiedCount = get_final_occupied_count(DataTable, RowNum, ColNum),
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

count_occupied_cells(DataTable, Cells) ->
  lists:foldl(fun(N, Acc) ->
    Acc + case ets:lookup(DataTable, N) of
            [] -> 0;
            [{_, floor}] -> 0;
            [{_, empty}] -> 0;
            [{_, occupied}] -> 1
          end
              end,
    0, Cells).

get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount) ->
  Neighbours = get_neighbours(CurrentRow, CurrentColumn, {RowCount, ColCount}),
  OccupiedNeighbourCount = count_occupied_cells(DataTable, Neighbours),
  OccupiedNeighbourCount.

tick(_DataTable, _TempTable, _Dimensions, [], [], ChangedCells) ->
  ChangedCells;
tick(DataTable, TempTable, {RowCount, ColCount}, [_CurrentRow | RemainingRows], [], ChangedCells) ->
  ColIndices = case RemainingRows of
                 [] -> [];
                 _ -> lists:seq(0, ColCount - 1)
               end,
  tick(DataTable, TempTable, {RowCount, ColCount}, RemainingRows, ColIndices, ChangedCells);
tick(DataTable, TempTable, {RowCount, ColCount}, Rows, [CurrentColumn | RemainingColumns], ChangedCells) ->
  [CurrentRow | _] = Rows,
  CurrentCell = ets:lookup(DataTable, {CurrentRow, CurrentColumn}),
  UpdatedChangedCells = case CurrentCell of
                          [{_, floor}] ->
                            ets:insert(TempTable, {{CurrentRow, CurrentColumn}, floor}),
                            ChangedCells;
                          [] ->
                            OccupiedNeighbourCount = get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount),
                            if OccupiedNeighbourCount =:= 0 ->
                              ets:insert(TempTable, {{CurrentRow, CurrentColumn}, occupied}),
                              ChangedCells + 1;
                              true -> ChangedCells
                            end;
                          _ ->
                            OccupiedNeighbourCount = get_occupied_neighbour_count(DataTable, {CurrentRow, CurrentColumn}, RowCount, ColCount),
                            if OccupiedNeighbourCount < 4 ->
                              ets:insert(TempTable, {{CurrentRow, CurrentColumn}, occupied}),
                              ChangedCells;
                              true -> ChangedCells + 1
                            end
                        end,
  tick(DataTable, TempTable, {RowCount, ColCount}, Rows, RemainingColumns, UpdatedChangedCells).

tick(DataTable, [RowCount, ColCount]) ->
  [RowIndices, ColIndices] = [lists:seq(0, RowCount - 1), lists:seq(0, ColCount - 1)],
  TempTable = ets:new(temp_table, [ordered_set]),
  ChangedCells = tick(DataTable, TempTable, {RowCount, ColCount}, RowIndices, ColIndices, 0),
  case ChangedCells of
    0 ->
      TempTable;
    CC ->
      io:fwrite("ChangedCells: ~p~n", [CC]), % useful progress indicator
      ets:delete(DataTable),
      tick(TempTable, [RowCount, ColCount])
  end.

get_final_occupied_count(DataTable, RowCount, ColCount) ->
  ResultTable = tick(DataTable, [RowCount, ColCount]),
  length(ets:match(ResultTable, {'$1', occupied})).

% dimensions 0-98, 0-91
