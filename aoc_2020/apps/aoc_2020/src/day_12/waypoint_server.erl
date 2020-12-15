%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(waypoint_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {position}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(InitialPosition) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [InitialPosition], []).

init([InitialPosition]) ->
  io:fwrite("Initing waypoint server... initial position=~p~n", [InitialPosition]),
  {ok, #state{position = InitialPosition}}.

handle_call({execute_instruction, {Verb, Value}, FerryPos}, _From, S = #state{}) ->
  NewPosition = calculate_new_position(S#state.position, {Verb, Value}, FerryPos),
  {reply, ok, S#state{position = NewPosition}};
handle_call({get_position}, _From, S = #state{}) ->
  {reply, S#state.position, S#state{}};
handle_call({update_position, Delta}, _From, S = #state{}) ->
  NewPosition = list_to_tuple(lists:zipwith(fun(A,B) -> A + B end,
    tuple_to_list(S#state.position), tuple_to_list(Delta))),
  {reply, ok, S#state{position = NewPosition}};
handle_call(_Request, _From, S = #state{}) ->
  {reply, ok, S}.

handle_cast(_Request, S = #state{}) ->
  {noreply, S}.

handle_info(_Info, S = #state{}) ->
  {noreply, S}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, S = #state{}, _Extra) ->
  {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_new_position(Point, {Verb, Value}, FerryPos) ->
  case Verb of
    V when V =:= n; V =:=e; V =:= s; V =:= w -> translate(Point, Verb, Value);
    V when V =:= l; V =:=r -> rotate(Point, FerryPos, Verb, Value)
  end.

translate({X, Y}, Direction, Amount) ->
  {DeltaX, DeltaY} = case Direction of
                       n -> {0, Amount};
                       e -> {Amount, 0};
                       s -> {0, -Amount};
                       w -> {-Amount, 0}
                     end,
  {X + DeltaX, Y + DeltaY}.
rotate_delta(Delta, _Direction, 0) ->
  Delta;
rotate_delta({Dx, Dy}, Direction, Iterations) ->
  case Direction of
    l -> rotate_delta({-Dy, Dx}, Direction, Iterations - 1);
    r -> rotate_delta({Dy, -Dx}, Direction, Iterations - 1)
  end.
rotate({X, Y}, {OriginX, OriginY}, Direction, Degrees) ->
  BaseIndex = floor(Degrees / 90),
  Delta = {X - OriginX, Y - OriginY},
  {RotatedDeltaX, RotatedDeltaY} = rotate_delta(Delta, Direction, BaseIndex),
  {OriginX + RotatedDeltaX, OriginY + RotatedDeltaY}.
