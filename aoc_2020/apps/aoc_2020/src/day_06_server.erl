%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_06_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_06).

-record(day_06_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #day_06_server_state{}}.

handle_call(_Request, _From, State = #day_06_server_state{}) ->
  {reply, ok, State}.

handle_cast({data_ready, part_01, ParsedData}, State = #day_06_server_state{}) ->
  io:fwrite("day_06, data_ready ~n"),
  tally_all_responses(ParsedData),
  {noreply, State};
handle_cast({data_ready, part_02, ParsedData}, State = #day_06_server_state{}) ->
  io:fwrite("day_06, part_02: data_ready ~n"),
  tally_unanimous_responses(ParsedData),
  {noreply, State};
handle_cast(_Request, State = #day_06_server_state{}) ->
  io:fwrite("day_06, unknown cast: ~w~n", [_Request]),
  {noreply, State}.

handle_info(part_01, State = #day_06_server_state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_06, part_01, self()}),
  {noreply, State};
handle_info(part_02, State = #day_06_server_state{}) ->
  io:fwrite("~p:handle_info. running part 02~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_06, part_02, self()}),
  {noreply, State};
handle_info(_Info, State = #day_06_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #day_06_server_state{}) ->
  ok.

code_change(_OldVsn, State = #day_06_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tally_group_responses([], Tally) ->
  Tally;
tally_group_responses([Group | RemainingResponses], Tally) ->
  YesResponses = lists:foldl(fun(PersonRecord, ResponseSet) ->
    lists:foldl(fun(Response, Acc) ->
      sets:add_element(Response, Acc)
                end, ResponseSet, PersonRecord)
                             end, sets:new(), Group),
  tally_group_responses(RemainingResponses, Tally + sets:size(YesResponses)).
tally_all_responses(ResponseData) ->
  io:fwrite("TotalResponses = ~p~n", [tally_group_responses(ResponseData, 0)]).

tally_unanimous_group_responses([], Tally) ->
  Tally;
tally_unanimous_group_responses([Group | RemainingResponses], Tally) ->
  UnanimousResponses = lists:foldl(fun(PersonRecord, Acc) ->
    Responses = sets:from_list(PersonRecord),
    case Acc of
              empty -> Responses;
              Set -> sets:intersection(Set, Responses)
            end
                                      end, empty, Group),
  tally_unanimous_group_responses(RemainingResponses, Tally + sets:size(UnanimousResponses)).
tally_unanimous_responses(ResponseData) ->
  io:fwrite("TotalResponses = ~p~n", [tally_unanimous_group_responses(ResponseData, 0)]).
