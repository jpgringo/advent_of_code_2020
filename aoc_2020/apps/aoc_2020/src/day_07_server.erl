%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_07_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, day_07).

-record(day_07_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #day_07_server_state{}}.

handle_call(_Request, _From, State = #day_07_server_state{}) ->
  {reply, ok, State}.

handle_cast({data_ready, part_01, ParsedData}, State = #day_07_server_state{}) ->
  io:fwrite("day_07, data_ready: ~p~n", [ParsedData]),
  TargetVertex = "shiny gold",
  AncestorList = get_ancestors(ParsedData, TargetVertex),
  io:fwrite("Ancestor List = ~p~n", [AncestorList]),
  io:fwrite("~p different bags can ultimately contain a ~p bag~n", [length(AncestorList), TargetVertex]),
  {noreply, State};
handle_cast({data_ready, part_02, ParsedData}, State = #day_07_server_state{}) ->
  io:fwrite("day_07, part_02 data_ready: ~p~n", [ParsedData]),
  TargetVertex = "shiny gold",
  {Tally, ChildList} = get_children(ParsedData, TargetVertex),
  io:fwrite(" a ~p bag can contain ~p bags in total, including: ~p~n", [TargetVertex, Tally, ChildList]),
  {noreply, State};
handle_cast(_Request, State = #day_07_server_state{}) ->
  {noreply, State}.

handle_info(part_01, State = #day_07_server_state{}) ->
  io:fwrite("~p:handle_info. running part 01~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_07, part_01, self()}),
  {noreply, State};
handle_info(part_02, State = #day_07_server_state{}) ->
  io:fwrite("~p:handle_info. running part 02~n", [?SERVER]),
  gen_server:cast(input_server, {parse, day_07, part_02, self()}),
  {noreply, State};
handle_info(_Info, State = #day_07_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #day_07_server_state{}) ->
  ok.

code_change(_OldVsn, State = #day_07_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_ancestors(_Graph, [], AncestorSet) ->
  AncestorSet;
get_ancestors(Graph, [Vertex | Rest], AncestorSet) ->
  OutNeighbours = digraph:out_neighbours(Graph, Vertex),
  NextTier = lists:foldl(fun(_Elem, AccIn) ->
    sets:union(AccIn, get_ancestors(Graph, [_Elem], sets:new()))
                         end,
    sets:new(), OutNeighbours),
  NewNeighbours = sets:union(sets:from_list(OutNeighbours), NextTier),
  UpdatedAncestors = sets:union(AncestorSet, NewNeighbours),
  get_ancestors(Graph, Rest, UpdatedAncestors).
get_ancestors(Graph, Vertex) ->
  lists:sort(sets:to_list(get_ancestors(Graph, [Vertex], sets:new()))).

get_children(_Graph, [], Tally, Children, _Depth) ->
  {Tally, Children};
get_children(Graph, [Edge | Rest], Tally, Children, Depth) ->
  {_, Vertex, _Parent, Count} = digraph:edge(Graph, Edge),
  InEdges = digraph:in_edges(Graph, Vertex),
  {ChildTally, ChildSet} = get_children(Graph, InEdges, 0, Children, Depth + 1),
  get_children(Graph, Rest, Tally + Count + Count * ChildTally, sets:union(sets:add_element(Vertex, Children),ChildSet), Depth).
get_children(Graph, Vertex) -> %hands off to the 'real' function
  InEdges = digraph:in_edges(Graph, Vertex),
  {Tally, ChildVertices} = get_children(Graph, InEdges, 0, sets:new(), 0),
  {Tally, lists:sort(sets:to_list(ChildVertices))}.
