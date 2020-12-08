%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(input_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(input_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:fwrite("input server inited~n"),
  {ok, #input_server_state{}}.

handle_call(_Request, _From, State = #input_server_state{}) ->
  {reply, ok, State}.

handle_cast({parse, PuzzleId, Segment, From}, State = #input_server_state{}) ->
  io:fwrite("input_server will load and parse file~n"),
  EnvVars = get_env_vars(),
  DownloadRoot = maps:get(input_root, EnvVars),
  SrcFilePath = filename:join(DownloadRoot, io_lib:format("~w_input_data.txt", [PuzzleId])),
  {ok, SrcFile} = file:open(SrcFilePath, [read]),
  case process_file(PuzzleId, SrcFile) of
    {err, Msg} -> io:fwrite("ERROR: ~s~n", [Msg]);
    {ok, ParsedData} ->
      gen_server:cast(From, {data_ready, Segment, ParsedData})
  end,
  {noreply, State};
handle_cast(_Request, State = #input_server_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #input_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #input_server_state{}) ->
  ok.

code_change(_OldVsn, State = #input_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env_vars(VarList) ->
  DefaultValues = #{},
  maps:merge(DefaultValues, maps:from_list(VarList)).
get_env_vars() ->
  get_env_vars(application:get_all_env()).

process_line(eof, _, _SrcFile, ParsedData) ->
  case ParsedData of
    PD when is_list(PD) -> {ok, lists:reverse(ParsedData)};
    _ -> {ok, ParsedData}
  end;
process_line(Line, day_05, SrcFile, ParsedData) ->
  {ok, Data} = Line,
  process_line(file:read_line(SrcFile), day_05, SrcFile, [string:chomp(Data) | ParsedData]);
process_line(Line, day_06, SrcFile, ParsedData) ->
  {ok, Data} = Line,
  TrimmedData = string:chomp(Data),
  UpdatedData = case length(TrimmedData) of
                  0 -> [[] | ParsedData];
                  _ ->
                    case ParsedData of
                      [] -> [[TrimmedData]];
                      PD -> [First | Rest] = PD,
                        [[TrimmedData | First] | Rest]
                    end
                end,
  process_line(file:read_line(SrcFile), day_06, SrcFile, UpdatedData);
process_line(Line, day_07, SrcFile, []) ->
  process_line(Line, day_07, SrcFile, digraph:new([acyclic]));
process_line(Line, day_07, SrcFile, ParsedData) ->
  {ok, Data} = Line,
  TrimmedData = string:chomp(Data),
  {ok, LineMP} = re:compile("(.*)\s+bags\s+contain\s+(.+)"),
  case re:run(TrimmedData, LineMP) of
    {match, [_, {ParentStart, ParentEnd}, {ChildStart, ChildEnd}]} ->
      ParentKey = string:slice(TrimmedData, ParentStart, ParentEnd),
      ChildString = string:slice(TrimmedData, ChildStart, ChildEnd),
      case re:run(ChildString, "([0-9]+)\s+([a-z]+\s+\[a-z]+)\s+bags?", [global]) of
        {match, ChildMatches} ->
          ChildData = lists:map(fun([_, {N1, N2}, {K1, K2}]) ->
            #{key => string:slice(ChildString, K1, K2),
              number => list_to_integer(string:slice(ChildString, N1, N2))}
                                end,
            ChildMatches),
          add_to_graph(ParsedData, ParentKey, ChildData);
        nomatch -> ok % this is a leaf node ("contain no other bags"); we don't need to do anything
      end;
    nomatch -> io:fwrite("no match: ~p~n", [TrimmedData])
  end,
  process_line(file:read_line(SrcFile), day_07, SrcFile, ParsedData);
process_line(_Line, UnknownPuzzleId, _SrcFile, _UpdatedData) ->
  {err, io_lib:format("I don't know how to process sources for ~p", [UnknownPuzzleId])}.
process_line(Line, PuzzleId, SrcFile) ->
  process_line(Line, PuzzleId, SrcFile, []).

process_file(PuzzleId, SrcFile) ->
  process_line(file:read_line(SrcFile), PuzzleId, SrcFile).

%%%===================================================================
%%% Puzzle-specific functions
%%%===================================================================

%%%% for day 07
add_to_graph(_Graph, _ParentKey, []) ->
  ok;
add_to_graph(Graph, ParentKey, [#{key := ChildKey, number := ChildNumber} | Rest]) ->
  digraph:add_vertex(Graph, ParentKey),
  digraph:add_vertex(Graph, ChildKey),
  digraph:add_edge(Graph, ChildKey, ParentKey, ChildNumber),
  add_to_graph(Graph, ParentKey, Rest).
