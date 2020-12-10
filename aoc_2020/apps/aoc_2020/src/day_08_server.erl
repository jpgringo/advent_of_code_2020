%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_08_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_08).

-record(day_08_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #day_08_server_state{}}.

handle_call(_Request, _From, State = #day_08_server_state{}) ->
  {reply, ok, State}.

handle_cast({data_ready, part_01, ParsedData}, State = #day_08_server_state{}) ->
  io:fwrite("day_08, data_ready...~n", []),
  Result = get_acc_before_loop(ParsedData),
  io:fwrite("Result=~p~n", [Result]),
  {noreply, State};
handle_cast(_Request, State = #day_08_server_state{}) ->
  {noreply, State}.

handle_info(part_01, State = #day_08_server_state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_08, part_01, self()}),
  {noreply, State};
handle_info(_Info, State = #day_08_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #day_08_server_state{}) ->
  ok.

code_change(_OldVsn, State = #day_08_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_acc_before_loop(_InstructionSet, error, Accumulator, _VisitedIndices) ->
  Accumulator;
get_acc_before_loop(InstructionSet, CurrentIndex, Accumulator, VisitedIndices) ->
  case lists:member(CurrentIndex, VisitedIndices) of
    true -> get_acc_before_loop(InstructionSet, error, Accumulator, VisitedIndices);
    false -> CurrentInstruction = lists:nth(CurrentIndex,InstructionSet),
      {NextIndex, NewValue} = case maps:get(op, CurrentInstruction) of
                    nop -> {CurrentIndex + 1,Accumulator};
                    jmp -> {CurrentIndex + maps:get(arg, CurrentInstruction),Accumulator};
                    acc -> {CurrentIndex + 1,Accumulator + maps:get(arg, CurrentInstruction)};
                    UnknownOp -> throw(io_lib:format("Unknown operation: ~p~n", [UnknownOp]))
                  end,
      get_acc_before_loop(InstructionSet, NextIndex, NewValue, [CurrentIndex | VisitedIndices])
  end.
get_acc_before_loop(InstructionSet) ->
  io:fwrite("Will check for infinite loop in instruction set~n", []),
  get_acc_before_loop(InstructionSet, 1, 0, []).
