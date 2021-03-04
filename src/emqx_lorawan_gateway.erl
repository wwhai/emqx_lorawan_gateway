-module(emqx_lorawan_gateway).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").
%%
%% Author:wwhai
%%
-behaviour(gen_server).

-export([start_link/0]).

-export([code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2]).

-export([ register_metrics/0
        , load/0
        , unload/0
        ]).
-export([on_message_publish/1]).

-define(LOG(Level, Format, Args), emqx_logger:Level("emqx_lorawan_gateway: " ++ Format, Args)).

-record(state, {baud_rate,
                data_bits,
                stop_bits,
                data_party,
                flow_control,
                device}).

register_metrics() ->
    emqx_metrics:new('emqx_lorawan_gateway.message_publish').

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}) ->
    {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}) ->
    {ok, Message}.

load() ->
    emqx:hook('client.message_publish', fun ?MODULE:on_message_publish/1, []).

unload() ->
    emqx:unhook('client.message_publish', fun ?MODULE:on_message_publish/1).

%%--------------------------------------------------------------------
%% gen_server
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([[BaudRate, DataBits, StopBits, DataParty, FlowControl, Device]]) ->
    case serctl:open(Device) of
        {error, Reason} ->
            io:format("Serial port: ~p open failed~n", [Device]),
            error({Reason, serial_port_can_not_open});
        {ok, FD} ->
            serctl:mode(raw),
            serctl:baud(BaudRate),
            io:format("Serial port: ~p open successfully~n", [Device]),
        {ok, #state{baud_rate = BaudRate,
                    data_bits = DataBits,
                    stop_bits = StopBits,
                    data_party = DataParty,
                    flow_control = FlowControl,
                    device = Device}}
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
