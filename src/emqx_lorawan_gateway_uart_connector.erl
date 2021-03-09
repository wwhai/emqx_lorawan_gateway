-module(emqx_lorawan_gateway_uart_connector).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").

%%
%% Author:wwhai
%%
-behaviour(gen_server).

-export([start_link/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

start_link({Name, Config}) ->
    EtsOptions = [named_table, public, set,
                 {keypos, #uart.device},
                 {write_concurrency, true},
                 {read_concurrency, true}],
    ?UART_TABLE = ets:new(?UART_TABLE, EtsOptions),
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
                    spawn_link(fun() ->
                        loop_receive_data(FD)
                    end),
            Uart = #uart{device = Device,
                            baud_rate = get_config("baud_rate", Config),
                            data_bits = get_config("data_bits", Config),
                            stop_bits = get_config("stop_bits", Config),
                            data_party = get_config("data_party", Config),
                            flow_control = get_config("flow_control", Config),
                            receive_buffer = get_config("receive_buffer", Config),
                            send_buffer = get_config("send_buffer", Config),
                            fd = FD,
                            status = work},
            ets:insert(?UART_TABLE, Uart),
            timer:send_interval(1000, self(), ping_slaver),
            {ok, Uart};
        _ ->
            error({error, Name ++ ":" ++ Device ++ " can't open!"})
    end.

loop_receive_data(FD) ->

    Type = read_data(FD, 1),
    case Type of
        %% Slaver --ping--> Master
        ?PING -> async_write(<<?PING_SUCCESS>>, FD);
        %% Master ---ping---> Slaver success
        ?PING_SUCCESS -> will_handle_ping_ok;
        %% Data from slaver
        ?DATA_SEND ->
            DataSize = read_data(FD, 2),
            Data = read_data(FD, DataSize),
            io:format("DATA_SEND => :~p~n", [Data]);
        %% Slaver received success
        ?DATA_RECEIVED_SUCCESS -> received_ok;
        _ -> ok
    end,
    loop_receive_data(FD).

handle_call({write, Data}, _From, State) ->
    Status = get_config(status, State),
    case Status of
        work ->
            ok = sync_write(get_config(fd, State), get_config(send_buffer, State), Data),
            {reply, ok, State};
        stop ->
            {reply, stop, State}
    end;

handle_call(Request, _From, State) ->
    io:format("Call => :~p~n", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    io:format("Cast => :~p~n", [Msg]),

    {noreply, State}.

% handle_info(ping_slaver, State) ->
%     FD = get_config(fd, State),
%     NewState1 = case sync_write(FD, <<?PING>>, 1) of
%         <<?PING_SUCCESS>> ->
%              NewState = State#{status := work},
%              ets:insert(?UART_TABLE, NewState), NewState;
%         _ ->
%              NewState = State#{status := stop},
%              ets:insert(?UART_TABLE, NewState), NewState
%     end,
%     {noreply, NewState1};

handle_info(Info, State) ->
    io:format("Info => :~p~n", [Info]),
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

sync_write(Data, FD, BufferSize) when is_binary(Data) ->
    case serctl:write(FD, Data) of
        ok -> read_data(FD, BufferSize);
        _ -> error
    end.

async_write(Data, FD) when is_binary(Data) ->
    R = serctl:write(FD, Data),
    case R of
        ok -> ok;
        _ -> error
    end.

flush_buffer(FD) ->
    case read_data(FD, 1) of
       <<>> -> ok;
       _Data -> flush_buffer(FD)
    end.

read_data(FD, Size) ->
    timer:sleep(?HARDWARE_DELAY_TIME),
    case serctl:read(FD, Size) of
       {ok, Data} -> Data;
        _ -> <<>>
    end.