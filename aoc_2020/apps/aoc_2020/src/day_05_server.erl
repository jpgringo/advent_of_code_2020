%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_05_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_05).
-define(F, "F").
-define(B, "B").

-record(day_04_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:fwrite("day_05_server inited~n"),
  {ok, #day_04_server_state{}}.

handle_call(_Request, _From, State = #day_04_server_state{}) ->
  {reply, ok, State}.

handle_cast({data_ready, part_01, ParsedData}, State = #day_04_server_state{}) ->
  io:fwrite("day_05, data_ready ~n"),
  find_highest_pass_id(ParsedData),
  {noreply, State};
handle_cast({data_ready, part_02, ParsedData}, State = #day_04_server_state{}) ->
  io:fwrite("day_05, data_ready ~n"),
  find_my_seat(ParsedData),
  {noreply, State};
handle_cast(_Request, State = #day_04_server_state{}) ->
  io:fwrite("day_05, unknown cast: ~w~n", [_Request]),
  {noreply, State}.

handle_info(part_01, State = #day_04_server_state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_05, part_01, self()}),
  {noreply, State};
handle_info(part_02, State = #day_04_server_state{}) ->
  io:fwrite("~p:handle_info. running part 02~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_05, part_02, self()}),
  {noreply, State};
handle_info(_Info, State = #day_04_server_state{}) ->
  io:fwrite("~p:handle_info. UNKNOWN MESSAGE: ~p~n", [?SERVER, _Info]),
  {noreply, State}.

terminate(_Reason, _State = #day_04_server_state{}) ->
  ok.

code_change(_OldVsn, State = #day_04_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_seat_id([], [], RowRange, ColumnRange) ->
  [RMin, RMax] = RowRange,
  [CMin, CMax] = ColumnRange,
  if RMin =/= RMax; CMin =/= CMax ->
    throw("Rows and columns must converge");
    true ->
%%      io:fwrite("Row=~p; Col=~p~n", [RMin, CMin]),
      RMin * 8 + CMin
  end;
get_seat_id([], [ColumnSector | ColumnData], RowRange, [ColumnMin, ColumnMax]) ->
  NewColumnRange = case [ColumnSector] of
                     "L" -> [ColumnMin, floor((ColumnMax - ColumnMin) / 2 + ColumnMin)];
                     "R" -> [ceil(ColumnMax - (ColumnMax - ColumnMin) / 2), ColumnMax]
                   end,
  get_seat_id([], ColumnData, RowRange, NewColumnRange);
get_seat_id([RowSector | RowData], _ColumnData, [RowMin, RowMax], _ColumnRange) ->
  NewRowRange = case [RowSector] of
                  "F" -> [RowMin, floor((RowMax - RowMin) / 2 + RowMin)];
                  "B" -> [ceil(RowMax - (RowMax - RowMin) / 2), RowMax]
                end,
  get_seat_id(RowData, _ColumnData, NewRowRange, _ColumnRange).
get_seat_id(Seat) ->
  get_seat_id(lists:sublist(Seat, 1, 7), lists:sublist(Seat, 8, 3), [0, 127], [0, 7]).


find_unoccupied_seats([], AvailableSeats) ->
  AvailableSeats;
find_unoccupied_seats([Seat | Rest], AvailableSeats) ->
  SeatId = get_seat_id(Seat),
  RemainingSeats = lists:delete(SeatId, AvailableSeats),
  find_unoccupied_seats(Rest, RemainingSeats).
find_unoccupied_seats(BoardingPasses) ->
  AvailableSeats = lists:seq(0, 128 * 8 - 1),
%%  io:fwrite("will find occupied seats from available seats: ~w~n", [AvailableSeats]),
  find_unoccupied_seats(BoardingPasses, AvailableSeats).


find_highest_pass_id([], HighestID) ->
  HighestID;
find_highest_pass_id([CurrentPass | Rest], HighestID) ->
  SeatId = get_seat_id(CurrentPass),
  find_highest_pass_id(Rest, max(SeatId, HighestID)).
find_highest_pass_id(BoardingPasses) ->
  HighestId = find_highest_pass_id(BoardingPasses, 0),
  io:fwrite("HighestID = ~p~n", [HighestId]).

find_isolated_seat(UnoccupiedSeats, _PreviousSeat) when length(UnoccupiedSeats) =:= 1 ->
  empty;
find_isolated_seat([SeatA | Rest], PreviousSeat) ->
  [SeatB | _] = Rest,
  case SeatA - PreviousSeat of
    Diff when Diff > 1, SeatB - SeatA > 1 ->
      SeatA;
    Diff when Diff =:= 1 ->
      find_isolated_seat(Rest, SeatA)
  end.
find_my_seat(BoardingPasses) ->
  UnoccupiedSeats = find_unoccupied_seats(BoardingPasses),
  [FirstSeat | OtherSeats] = UnoccupiedSeats,
  IsolatedSeat = find_isolated_seat(OtherSeats, FirstSeat),
  io:fwrite("IsolatedSeat: ~p~n", [IsolatedSeat]),
  ok.
