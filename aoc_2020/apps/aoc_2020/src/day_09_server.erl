%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_09_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_09).

-record(day_09_server_state, {head_size}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #day_09_server_state{head_size = 25}}.

handle_call(_Request, _From, State = #day_09_server_state{}) ->
  {reply, ok, State}.

handle_cast({data_ready, part_01, ParsedData}, State = #day_09_server_state{}) ->
  io:fwrite("day_09, data_ready...~n", []),
  Result = find_first_invalid_entry(ParsedData, State#day_09_server_state.head_size),
  io:fwrite("~p is the first number for which addends could not be found~n", [Result]),
  {noreply, State};
handle_cast({data_ready, part_02, ParsedData}, State = #day_09_server_state{}) ->
  io:fwrite("day_09, data_ready...~n", []),
  EncryptionWeakness = find_encryption_weakness(ParsedData, State#day_09_server_state.head_size),
  io:fwrite("Encryption weakness = ~p~n", [EncryptionWeakness]),
  {noreply, State};
handle_cast(_Request, State = #day_09_server_state{}) ->
  {noreply, State}.

handle_info(part_01, State = #day_09_server_state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_09, part_01, self()}),
  {noreply, State};
handle_info(part_02, State = #day_09_server_state{}) ->
  io:fwrite("~p:handle_info. running part 02~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_09, part_02, self()}),
  {noreply, State};
handle_info(_Info, State = #day_09_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #day_09_server_state{}) ->
  ok.

code_change(_OldVsn, State = #day_09_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_first_invalid_entry([], _SplitPoint) ->
  ok;
find_first_invalid_entry(Entries, HeadSize = 25) ->
  if length(Entries) =< HeadSize ->
    find_first_invalid_entry([], HeadSize);
    true ->
      {Head, [TestEntry | _Tail]} = lists:split(HeadSize, Entries),
      case find_addends(TestEntry, Head) of
        {ok, _A1, _A2} ->
          find_first_invalid_entry(lists:nthtail(1, Entries), HeadSize);
        {error, InvalidEntry} ->
          InvalidEntry
      end
  end.

find_addends(_Sum, _FirstAddend, []) ->
  error;
find_addends(Sum, FirstAddend, [Candidate | Remaining]) ->
  if FirstAddend + Candidate =:= Sum ->
    {ok, FirstAddend, Candidate};
    true -> find_addends(Sum, FirstAddend, Remaining)
  end.
find_addends(Sum, []) ->
  {error, Sum};
find_addends(Sum, [FirstAddend | RemainingAddends]) ->
  case find_addends(Sum, FirstAddend, RemainingAddends) of
    {ok, Addend1, Addend2} -> {ok, Addend1, Addend2};
    error ->
      find_addends(Sum, RemainingAddends)
  end.

find_addend_group(TargetSum, AccumulatedAddends, [NextAddend | RemainingAddends], RunningTotal) ->
  UpdatedTotal = RunningTotal + NextAddend,
  case UpdatedTotal of
    Sum when Sum < TargetSum ->
      find_addend_group(TargetSum, [NextAddend | AccumulatedAddends], RemainingAddends, UpdatedTotal);
    Sum when Sum =:= TargetSum -> {ok, AccumulatedAddends};
    Sum when Sum > TargetSum -> {error, "TargetSum exceeded"}
  end.
find_addend_group(TargetSum, Entries) ->
  [First | Remaining] = Entries,
  case find_addend_group(TargetSum, [First], Remaining, First) of
    {error, _Msg} -> find_addend_group(TargetSum, lists:nthtail(1, Entries));
    {ok, ValidAddends} -> ValidAddends
  end.

find_encryption_weakness(Entries, HeadSize) ->
  Result = find_first_invalid_entry(Entries, HeadSize),
  AddendGroup = find_addend_group(Result, Entries),
  SortedGroup = lists:sort(AddendGroup),
  [Min | _] = SortedGroup,
  Max = lists:last(SortedGroup),
  Min + Max.
