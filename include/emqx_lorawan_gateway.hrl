
-define(APP, emqx_lorawan_gateway).

%% Some low performance hardware may have UART delay
-define(HARDWARE_DELAY_TIME, 10).

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
               status::atom()}).