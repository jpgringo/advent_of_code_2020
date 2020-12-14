%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_12_server).

-behaviour(gen_server).

-include("../specs.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_12).
-define(DAY_12_FERRY_SPEC(FerryId, InitialState), ?PUZZLE_STATEM_SPEC(FerryId, ferry_statem, [InitialState])).


-record(state, {start_time, sup}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Sup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Sup], []).

init([Sup]) ->
  io:fwrite("Inited ~p; Sup=~p~n", [?SERVER, Sup]),
  {ok, #state{sup = Sup}}.

handle_call({process_data, Line, AccumulatedData}, _From, S = #state{}) ->
  UpdatedData = process_data(Line, AccumulatedData),
  {reply, {ok, UpdatedData}, S};
handle_call(_Request, _From, S = #state{}) ->
  {reply, ok, S}.

handle_cast({data_ready, part_01, InstructionList}, S = #state{}) ->
  io:fwrite("day_12, data_ready... ~n", []),
  FerryId = day_12_ferry,
  {ok, FerryPid} =
    supervisor:start_child(S#state.sup, ?DAY_12_FERRY_SPEC(FerryId, {east, {0, 0}})),
  ManhattanDistance = get_manhattan_distance(FerryPid, InstructionList),
  io:fwrite("Manhattan distance=~p~n", [ManhattanDistance]),
  supervisor:terminate_child(S#state.sup, day_12_ferry),
  supervisor:delete_child(S#state.sup, day_12_ferry),
  {noreply, S};
handle_cast(_Request, S = #state{}) ->
  {noreply, S}.

handle_info(part_01, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_12, part_01, {process_data, []}, self()}),
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

process_data([Verb | ValueString], AccumulatedData) ->
  Value = case string:to_integer(ValueString) of
            {error, _} -> throw(io_lib:format("Invalid integer string: ~s", [ValueString]));
            {Int, _} -> Int
          end,
  [{list_to_atom(string:to_lower([Verb])), Value} | AccumulatedData].

navigate(_Ferry, [], LastState) ->
  LastState;
navigate(Ferry, [Instruction | FurtherInstructions], _LastState) ->
  % I want these synchronous so the state query at the end doesn't miss pending instructions
  NewState = gen_statem:call(Ferry, Instruction),
  navigate(Ferry, FurtherInstructions, NewState).
get_manhattan_distance(Ferry, InstructionList) ->
  {Facing, Position} = navigate(Ferry, InstructionList, {}),
  io:fwrite("All instructions followed.~n", []),
  io:fwrite("Final state: facing ~p at ~p~n", [Facing, Position]),
  {X,Y} = Position,
  abs(X) + abs(Y). %% original position assumed to be {0,0}, for now
