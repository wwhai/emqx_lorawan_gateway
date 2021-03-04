%%%-------------------------------------------------------------------
%% @doc emqx_lorawan_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(emqx_lorawan_gateway_app).

-behaviour(application).

-include("emqx_lorawan_gateway.hrl").

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        , start_uart/1
        ]).
start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_lorawan_gateway_sup:start_link(),
    UartsConfigs = application:get_env(?APP, uart_configs, []),
    lists:foreach(fun start_uart/1, UartsConfigs),
    ?APP:load(),
    ?APP:register_metrics(),
    {ok, Sup}.

stop(_State) ->
    ?APP:unload(),
    ok.

start_uart({Name, [_, _, _, _, _, DeviceName] = Config}) ->
    _Pid = proc_lib:spawn_link(list_to_atom(Name ++ DeviceName), init, [Config]).