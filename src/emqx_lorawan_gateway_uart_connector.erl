-module(emqx_lorawan_gateway_uart_connector).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").
-define(PING_PACKET, 0).
%%
%% Author:wwhai
%%
-behaviour(gen_server).

-export([start_link/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

start_link({Name, Config}) ->
    application:ensure_all_started(srly),
    gen_server:start_link({local, Name}, ?MODULE, [{Name, Config}], []).

init([{Name, Config}]) ->
    Device = get_config("device", Config),
    case serctl:open(Device) of
        {ok, FD} ->
            io:format("Serial port: ~p open successfully~n", [Device]),
            Termios = lists:foldl(
                fun(Fun, Acc) -> Fun(Acc) end,
                serctl:mode(raw),
                [
                    fun(N) -> serctl:flow(N, false) end,
                    fun(N) -> serctl:ispeed(N, get_config("baud_rate", Config, 115200)) end,
                    fun(N) -> serctl:ospeed(N, get_config("baud_rate", Config, 115200)) end
                ]
            ),
            ok = serctl:tcsetattr(FD, tcsanow, Termios),
            timer:send_interval(1000, self(), {ping, ?PING_PACKET}),
            {ok, #{baud_rate => get_config("baud_rate", Config),
                   data_bits => get_config("data_bits", Config),
                   stop_bits => get_config("stop_bits", Config),
                   data_party => get_config("data_party", Config),
                   flow_control => get_config("flow_control", Config),
                   device => Device,
                   fd => FD,
                   status => work}};
        _ ->
            error({error, Name ++ ":" ++ Device ++ " can't open!"})
    end.

handle_call({write, Data}, _From, State) ->
    Status = get_config(status, State),
    case Status of
        work ->
            ok = send_data(get_config(fd, State), Data),
            {reply, ok, State};
        stop ->
            {reply, stop, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ping, PingPacket}, State) ->
    FD = get_config(fd, State),
    NewState = case send_data(FD, <<PingPacket>>) of
        ok -> State#{status := work};
        _ -> State#{status := stop}
    end,
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("Received => :~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Private function
%%-----------------------------------------------------------------------------
get_config(Key, Config) ->
    proplists:get_value(Key, Config).

get_config(Key, Config, Default) ->
    proplists:get_value(Key, Config, Default).

%%-----------------------------------------------------------------------------
%% API function
%%-----------------------------------------------------------------------------

send_data(Data, FD) when is_binary(Data) ->
    R = serctl:write(FD, Data),
    case R of
        ok -> ok;
        _ -> error
    end.

read_data(FD, Size) ->
    case serctl:read(FD, Size) of
       {ok, Data} -> Data;
        _ -> <<>>
    end.