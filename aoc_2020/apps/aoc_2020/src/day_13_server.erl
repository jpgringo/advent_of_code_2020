%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_13_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_13).

-record(state, {start_time}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:fwrite("initing day 13 server...~n", []),
  {ok, #state{}}.

handle_call({process_data, Line, AccumulatedData}, _From, S = #state{}) ->
  UpdatedData = process_data(Line, AccumulatedData),
  {reply, {ok, UpdatedData}, S};
handle_call(_Request, _From, S = #state{}) ->
  {reply, ok, S}.

handle_cast({data_ready, part_01, {ArrivalEstimate, BusIds, _}}, S = #state{}) ->
  io:fwrite("day_13, data_ready... ~n"),
  {Id, Diff} = get_earliest_possible_bus(ArrivalEstimate, BusIds),
  TimeDiff = erlang:system_time(millisecond) - S#state.start_time,
  io:fwrite("Earliest possible bus is #~p, ~pmins after arrival, for a product of ~p. ",
    [Id, Diff, Id * Diff]),
  io:fwrite("Execution time=~pms~n", [TimeDiff]),
  {noreply, S};
handle_cast(_Request, S = #state{}) ->
  {noreply, S}.

handle_info(part_01, S = #state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server_2, {parse, day_13, part_01, {process_data, {0, [], false}}, self()}),
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

process_data(Line, {TimeEstimate, BusIds, EstimateCaptured}) ->
  case EstimateCaptured of
    false -> {Est,_} = string:to_integer(Line), {Est, BusIds, true};
    true -> Entries = lists:map(fun(A) -> {Int, _} = string:to_integer(A), Int end, re:split(Line, "[x,]+", [{return,list}])),
      {TimeEstimate, lists:append(BusIds, Entries), EstimateCaptured}
  end.

get_earliest_possible_bus(_ArrivalEstimate, [], BestResult) ->
  BestResult;
get_earliest_possible_bus(ArrivalEstimate, [BusId | RemainingIds], BestResult) ->
  Factor = ceil(ArrivalEstimate / BusId),
  Diff = Factor * BusId - ArrivalEstimate,
  NewBest = case BestResult of
              undefined -> {BusId, Diff};
              {_, D} when Diff < D -> {BusId, Diff};
              BR -> BR
            end,
  get_earliest_possible_bus(ArrivalEstimate, RemainingIds, NewBest).
get_earliest_possible_bus(ArrivalEstimate, BusIds) ->
  io:fwrite("will get earliest possible bus based on: arrival=~p, buses=~p~n", [ArrivalEstimate, BusIds]),
  get_earliest_possible_bus(ArrivalEstimate, BusIds, undefined).

