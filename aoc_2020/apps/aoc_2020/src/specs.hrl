%%%-------------------------------------------------------------------
%%% @author rick
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2020 22:59
%%%-------------------------------------------------------------------
-author("rick").

-define(PUZZLE_SUP_SPEC(SupId, Module, Args),
  #{id => SupId,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => []}).
-define(PUZZLE_SERVER_SPEC(ServerId, Module, Args),
  #{id => ServerId,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => []}).
-define(PUZZLE_STATEM_SPEC(StatemId, Module, Args),
  #{id => StatemId,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => []}).
