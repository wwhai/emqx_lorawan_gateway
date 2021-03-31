
-define(APP, emqx_lorawan_gateway).

%% Some low performance hardware may have UART delay
%% Default is 100ms
-define(HARDWARE_DELAY_TIME, 100).
%% Heart beat timeout is 5000ms
-define(HEART_BEAT_TIMEOUT, 5000).

-define(PING, 16#00).

-define(PING_SUCCESS, 16#01).

-define(DATA_SEND, 16#02).

-define(DATA_RECEIVED_SUCCESS, 16#03).

-define(UNKNOWN_PACKET, 16#04).

-define(ERROR, 16#05).


-define(UART_TABLE, uart_table).

-record(uart, {device::string(),
               baud_rate::integer(),
               data_bits::integer(),
               stop_bits::integer(),
               data_party::atom(),
               flow_control::atom(),
               receive_buffer::integer(),
               send_buffer::integer(),
               fd::port(),
               status::work | stop |busy}).

-define(MSG_ID_LENGTH, 2).

-define(MSG_TYPE_LENGTH, 1).

-define(MSG_LENGTH, 2).