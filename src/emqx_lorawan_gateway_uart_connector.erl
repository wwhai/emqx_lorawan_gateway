-module(emqx_lorawan_gateway_uart_connector).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

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
    gen_msg_id(),
    case serctl:open(Device) of
        {ok, FD} ->
            io:format("Serial port: ~p open successfully~n", [Device]),
            % flush_buffer(FD),
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
            _Pid = proc_lib:spawn_link(fun() ->
                receive_data(FD)
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
            timer:send_interval(1000, self(), ping_slaver),
            {ok, Uart};
        _ ->
            error({error, Name ++ ":" ++ Device ++ " can't open!"})
    end.

receive_data(FD) ->
    case read_data(FD, ?MSG_ID_LENGTH) of
        <<>> -> ok;
        <<MsgId:16, _/binary>> = Bin ->
            io:format("MsgId => :~p~n", [Bin]),
            <<Type:8, _/binary>> = read_data(FD, ?MSG_TYPE_LENGTH),
            io:format("Type => :~p~n", [Type]),
            case Type of
                %% Slaver --ping--> Master
                ?PING -> handle_ping(MsgId);
                %% Master ---ping---> Slaver success
                ?PING_SUCCESS -> handle_ping_slaver_success(MsgId);%% update status to work.
                %% Data from slaver
                ?DATA_SEND -> handle_data_send(MsgId);%% return success to slaver
                %% Slaver received success
                ?DATA_RECEIVED_SUCCESS -> handle_receive_success(MsgId); %% remove msgid
                %% unknown packet
                _ -> handle_unknown_packet(MsgId)
            end
    end.

%% ping from slaver
handle_call({ping, MsgId}, _From, #uart{fd = FD} = State) ->
    ct:print("|>=> :~p~n", [<<MsgId:24, ?PING_SUCCESS:8>>]),
    sync_write(<<MsgId:24, ?PING_SUCCESS:8>>, FD),
    {reply, ok, State};
%% ping slaver success
handle_call({ping_slaver_success, MsgId}, _From, #uart{fd = FD} = State) ->
    %% remove ping packet id
    {reply, ok, State#uart{status = work}};
%% data from slaver
handle_call({data_send, MsgId}, _From, #uart{fd = FD} = State) ->
    DataSize = read_data(FD, ?MSG_LENGTH),
    Data = read_data(FD, DataSize),
    io:format("Data from slaver => :~p~n", [Data]),
    sync_write(<<MsgId:24, ?DATA_RECEIVED_SUCCESS>>, FD),
    {reply, ok, State};
%% received data
handle_call({receive_success, MsgId}, _From, #uart{fd = FD} = State) ->
    %% remove send data packet id
    {reply, ok, State};
%% unknown packet
handle_call({unknown_packet, MsgId}, _From, #uart{fd = FD} = State) ->
    sync_write(<<MsgId:24, ?UNKNOWN_PACKET>>, FD),
    {reply, ok, State};

handle_call(_Request, _From, #uart{fd = FD} = State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ping_slaver, #uart{fd = FD} = State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% API function
%%-----------------------------------------------------------------------------
get_config(Key, Config) ->
    proplists:get_value(Key, Config).

get_config(Key, Config, Default) ->
    proplists:get_value(Key, Config, Default).

sync_write(Data, FD) when is_binary(Data) ->
    case serctl:write(FD, Data) of
        ok -> ok;
        _ -> error
    end.

flush_buffer(FD) ->
    case read_data(FD, 1) of
       <<>> -> ok;
       _ -> flush_buffer(FD)
    end.

read_data(FD, Size) ->
    case serctl:read(FD, Size) of
        {error, _} -> <<>>;
        {ok, Bin} -> io:format("read_data => :~p~n", [{size, Size, bin, Bin}]), Bin
    end.

gen_msg_id() ->
    case get(atom_counter) of
        undefined -> 0;
        N -> put(atom_counter, N+1), N+1
    end.

handle_ping(MsgId) ->
    gen_server:call(self(), {ping, MsgId}).

handle_ping_slaver_success(MsgId) ->
    gen_server:call(self(), {ping_slaver_success, MsgId}).

handle_data_send(MsgId) ->
    gen_server:call(self(), {data_send, MsgId}).

handle_receive_success(MsgId) ->
    gen_server:call(self(), {receive_success, MsgId}).

handle_unknown_packet(MsgId) ->
    gen_server:call(self(), {unknown_packet, MsgId}).
