%% @author: 
%% @description: 
-module(emqx_lorawan_gateway_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
%%--------------------------------------------------------------------
%% Setups
%%--------------------------------------------------------------------
all() ->
    emqx_ct_helpers:start_apps([emqx_lorawan_gateway]),
    emqx_ct:all(?MODULE).

init_per_suite(_Cfg) ->
    _Cfg.

end_per_suite(_) ->
    ok.

init_per_group(_Group , _Cfg) ->
    ok.

end_per_group(_Group, _Cfg) ->
    ok.
%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------
t_test(_) ->
    ok.
