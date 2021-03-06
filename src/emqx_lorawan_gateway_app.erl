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
        ]).
start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_lorawan_gateway_sup:start_link(),
    ?APP:load(),
    ?APP:register_metrics(),
    {ok, Sup}.

stop(_State) ->
    ?APP:unload(),
    ok.
