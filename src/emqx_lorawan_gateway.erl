-module(emqx_lorawan_gateway).

-include("emqx_lorawan_gateway.hrl").
-include_lib("emqx/include/emqx.hrl").
%%
%% Author:wwhai
%%

-define(LOG(Level, Format, Args), emqx_logger:Level("emqx_lorawan_gateway: " ++ Format, Args)).

-export([ register_metrics/0
        , load/0
        , unload/0
        ]).

-export([on_message_publish/1]).

%%--------------------------------------------------------------------
%% Plugin function
%%--------------------------------------------------------------------

register_metrics() ->
    emqx_metrics:new('emqx_lorawan_gateway.message_publish').

load() ->
    emqx:hook('client.message_publish', fun ?MODULE:on_message_publish/1, []).

unload() ->
    emqx:unhook('client.message_publish', fun ?MODULE:on_message_publish/1).

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}) ->
    {ok, Message};

on_message_publish(Message) ->
    {ok, Message}.
