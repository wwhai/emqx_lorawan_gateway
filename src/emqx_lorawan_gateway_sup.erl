%%%-------------------------------------------------------------------
%% @doc emqx_lorawan_gateway top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(emqx_lorawan_gateway_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    UartsConfigs = application:get_env(emqx_lorawan_gateway, uart_configs, []),
    ChildrenSpecs = lists:map(fun({Name, Config}) ->
        #{id => list_to_atom(Name),
          start => {emqx_lorawan_gateway_uart_connector, start_link, [{list_to_atom(Name), Config}]},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [emqx_lorawan_gateway_uart_connector]}
    end, UartsConfigs),
    {ok, {{one_for_one, 10, 100}, ChildrenSpecs}}.