%%%-------------------------------------------------------------------
%% @doc aoc_2020 public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc_2020_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:fwrite("Yup, we're running!~n"),
  aoc_2020_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================