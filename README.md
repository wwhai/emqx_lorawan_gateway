emqx_lorawan_gateway
========

An EMQ X plugin

##### emqx_lorawan_gateway.conf

```properties
emqx_lorawan_gateway.hook.client.connected.1     = {"action": "on_client_connected"}
emqx_lorawan_gateway.hook.client.disconnected.1  = {"action": "on_client_disconnected"}
emqx_lorawan_gateway.hook.client.subscribe.1     = {"action": "on_client_subscribe"}
emqx_lorawan_gateway.hook.client.unsubscribe.1   = {"action": "on_client_unsubscribe"}
emqx_lorawan_gateway.hook.session.subscribed.1   = {"action": "on_session_subscribed"}
emqx_lorawan_gateway.hook.session.unsubscribed.1 = {"action": "on_session_unsubscribed"}
emqx_lorawan_gateway.hook.message.publish.1      = {"action": "on_message_publish"}
emqx_lorawan_gateway.hook.message.delivered.1    = {"action": "on_message_delivered"}
emqx_lorawan_gateway.hook.message.acked.1        = {"action": "on_message_acked"}
```

License
-------

Apache License Version 2.0

Author
------

Contributors
------------

