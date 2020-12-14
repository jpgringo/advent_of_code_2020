%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(day_12_sup).

-behaviour(supervisor).

-include("../specs.hrl").

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%  AChild = #{id => 'AName',
%%    start => {'AModule', start_link, []},
%%    restart => permanent,
%%    shutdown => 2000,
%%    type => worker,
%%    modules => ['AModule']},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [?PUZZLE_SUP_SPEC(day_12_server,day_12_server, [self()])]}
  }.
