%%%-------------------------------------------------------------------
%% @doc aoc_2020 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aoc_2020_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(PUZZLE_SERVER_SPEC (ServerId, Module, Args),
  #{id => ServerId,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => []}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  InputServerSpec = #{id => input_server,
    start => {input_server, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => []},
  InputServerSpec2 = #{id => input_server_2,
    start => {input_server_2, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => []},
  Procs = [
    InputServerSpec,
    InputServerSpec2,
    ?PUZZLE_SERVER_SPEC(day_04_server,day_04_server, []),
    ?PUZZLE_SERVER_SPEC(day_05_server,day_05_server, []),
    ?PUZZLE_SERVER_SPEC(day_06_server,day_06_server, []),
    ?PUZZLE_SERVER_SPEC(day_07_server,day_07_server, []),
    ?PUZZLE_SERVER_SPEC(day_08_server,day_08_server, []),
    ?PUZZLE_SERVER_SPEC(day_09_server,day_09_server, []),
    ?PUZZLE_SERVER_SPEC(day_10_server,day_10_server, [])
  ],
    {ok, {{one_for_all, 0, 1}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
