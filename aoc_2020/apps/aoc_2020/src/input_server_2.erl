%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(input_server_2).

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

handle_cast({parse, PuzzleId, Segment, {ParseFunction, InitialData}, From}, State = #input_server_state{}) ->
  io:fwrite("input_server will load and parse file using parse function ~p~n", [ParseFunction]),
  EnvVars = get_env_vars(),
  DownloadRoot = maps:get(input_root, EnvVars),
  SrcFilePath = filename:join(DownloadRoot, io_lib:format("~w_input_data.txt", [PuzzleId])),
  {ok, SrcFile} = file:open(SrcFilePath, [read]),
  case process_file(SrcFile, InitialData, ParseFunction, From) of
    {err, Msg} -> io:fwrite("ERROR: ~s~n", [Msg]);
    {ok, ParsedData} ->
      if is_list(ParsedData) -> lists:reverse(ParsedData);
        true -> ParsedData
      end,
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

process_line(eof, _SrcFile, ParsedData, _LineFunction, _Caller) ->
  case ParsedData of
    PD when is_list(PD) -> {ok, lists:reverse(ParsedData)};
    _ -> {ok, ParsedData}
  end;
process_line(Line, SrcFile, ParsedData, LineFunction, Caller) ->
  {ok, Data} = Line,
  {ok, UpdatedData} = gen_server:call(Caller, {LineFunction, string:chomp(Data), ParsedData}),
  process_line(file:read_line(SrcFile), SrcFile, UpdatedData, LineFunction, Caller).

process_file(SrcFile, InitialData, LineFunction, Caller) ->
  io:fwrite("process_file(~p, ~p, ~p)~n", [SrcFile, LineFunction, Caller]),
  process_line(file:read_line(SrcFile), SrcFile, InitialData, LineFunction, Caller).
