%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2020 23:10
%%%-------------------------------------------------------------------
-module(ferry_statem).
-author("rick").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%state_functions
-export([north/3, west/3, south/3, east/3]).

-define(SERVER, ?MODULE).
-define(NEXT_STATE_TEMPLATE, "  -->  NextState=~p, Delta={~p,~p}, UpdatedPosition=~p~n").
-record(state, {position}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(InitialValues) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [InitialValues], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([{InitialDirection, InitialPosition}]) ->
  {ok, InitialDirection, #state{position = InitialPosition}}.

callback_mode() ->
  state_functions.

format_status(_Opt, [_PDict, _StateName, _S]) ->
  Status = some_term,
  Status.

%%%===================================================================
%%% state functions
%%%===================================================================

north({call, From}, {Verb, Value}, S = #state{}) ->
  {NextStateName, {DeltaX, DeltaY}} = case Verb of
                                        l -> {rotate(north, left, Value), {0, 0}};
                                        f -> {north, {0, Value}};
                                        r -> {rotate(north, right, Value), {0, 0}};
                                        n -> {north, {0, Value}};
                                        e -> {north, {Value, 0}};
                                        s -> {north, {0, -Value}};
                                        w -> {north, {-Value, 0}}
                                      end, east,
  {CurrentX, CurrentY} = S#state.position,
  UpdatedPosition = {CurrentX + DeltaX, CurrentY + DeltaY},
  {next_state, NextStateName,
    S#state{position = UpdatedPosition}, {reply, From, {NextStateName, UpdatedPosition}}}.
west({call, From}, {Verb, Value}, S = #state{}) ->
  {NextStateName, {DeltaX, DeltaY}} = case Verb of
                                        l -> {rotate(west, left, Value), {0, 0}};
                                        f -> {west, {-Value, 0}};
                                        r -> {rotate(west, right, Value), {0, 0}};
                                        n -> {west, {0, Value}};
                                        e -> {west, {Value, 0}};
                                        s -> {west, {0, -Value}};
                                        w -> {west, {-Value, 0}}
                                      end, east,
  {CurrentX, CurrentY} = S#state.position,
  UpdatedPosition = {CurrentX + DeltaX, CurrentY + DeltaY},
  {next_state, NextStateName,
    S#state{position = UpdatedPosition}, {reply, From, {NextStateName, UpdatedPosition}}}.
south({call, From}, {Verb, Value}, S = #state{}) ->
  {NextStateName, {DeltaX, DeltaY}} = case Verb of
                                        l -> {rotate(south, left, Value), {0, 0}};
                                        f -> {south, {0, -Value}};
                                        r -> {rotate(south, right, Value), {0, 0}};
                                        n -> {south, {0, Value}};
                                        e -> {south, {Value, 0}};
                                        s -> {south, {0, -Value}};
                                        w -> {south, {-Value, 0}}
                                      end, east,
  {CurrentX, CurrentY} = S#state.position,
  UpdatedPosition = {CurrentX + DeltaX, CurrentY + DeltaY},
  {next_state, NextStateName,
    S#state{position = UpdatedPosition}, {reply, From, {NextStateName, UpdatedPosition}}}.
east({call, From}, {Verb, Value}, S = #state{}) ->
  {NextStateName, {DeltaX, DeltaY}} = case Verb of
                                        l -> {rotate(east, left, Value), {0, 0}};
                                        f -> {east, {Value, 0}};
                                        r -> {rotate(east, right, Value), {0, 0}};
                                        n -> {east, {0, Value}};
                                        e -> {east, {Value, 0}};
                                        s -> {east, {0, -Value}};
                                        w -> {east, {-Value, 0}}
                                      end, east,
  {CurrentX, CurrentY} = S#state.position,
  UpdatedPosition = {CurrentX + DeltaX, CurrentY + DeltaY},
  {next_state, NextStateName,
    S#state{position = UpdatedPosition}, {reply, From, {NextStateName, UpdatedPosition}}}.

%% @private
%% Only needed if callback_mode is handle_event_function
handle_event(_EventType, _EventContent, _StateName, S = #state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, S}.

terminate(_Reason, _StateName, _S = #state{}) ->
  ok.

code_change(_OldVsn, StateName, S = #state{}, _Extra) ->
  {ok, StateName, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rotate(StartingDirection, _RotationDirection, Degrees) ->
  NewDirection = case Degrees of
                   0 -> StartingDirection;
                   _ -> DirectionList = [north, west, south, east],
                     Offset = case StartingDirection of
                                north -> 0;
                                west -> 1;
                                south -> 2;
                                east -> 3
                              end,
                     BaseIndex = floor(Degrees / 90),
                     DirectionCount = length(DirectionList),
                     ActualIndex =  case _RotationDirection of
                                      left -> ((BaseIndex + Offset) rem DirectionCount) + 1;
                                      right ->
                                        ((BaseIndex + ((DirectionCount - 1) - Offset)) rem DirectionCount) + 1
                                    end,
                     lists:nth(ActualIndex, case _RotationDirection of
                                              left -> DirectionList;
                                              right -> lists:reverse(DirectionList)
                                            end)
                 end,
  NewDirection.
