-module(emqx_lorawan_gateway_cli).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").

%%
%% Author:wwhai
%%
-behaviour(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

init([{ _, [BaudRate, DataBits, StopBits, DataParty, FlowControl, Device]}]) ->
    case serctl:open(Device) of
        {ok, FD} ->
            io:format("Serial port: ~p open successfully~n", [Device]),
            serctl:mode(raw),
            serctl:baud(BaudRate),
            {ok, #{baud_rate => BaudRate,
                   data_bits => DataBits,
                   stop_bits => StopBits,
                   data_party => DataParty,
                   flow_control => FlowControl,
                   device => Device,
                   fd => FD}};
        _ ->
            error({error, serial_port_can_not_open})
    end.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.