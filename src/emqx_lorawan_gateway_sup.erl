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
    _ = lists:map(fun({Name, [_, _, _, _, _, Device]} = Config) ->
        {ok, _} = gen_server:start_link({global, list_to_atom(Name ++ Device)}, emqx_lorawan_gateway_cli, [Config], [])
    end, UartsConfigs),
    {ok, {{one_for_one, 10, 100}, []}}.