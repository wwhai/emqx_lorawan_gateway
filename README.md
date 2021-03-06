# emqx lorawan gateway
EMQX lorawan gateway plugin.

## Quick start
Add plugin to your emqx.

## Config

- Serial baud rate, default 115200
- Data bits, default 8
- Data stop bits, default 1
- Data party, default none
- Serial flowv control, default none

Config format: BaudRate, DataBits, StopBits, DataParty, FlowControl, Device

```ini
emqx_lorawan_gateway.uart.1 = 115200, 8, 1, none, none, /dev/cu.Bluetooth-Incoming-Port
```
## License
MIT
## Author
- wwhai

## community
- QQ Group: 475512169
- HomePage1: http://openlab.ezlinker.cn
- HomePage2: http://wwhai.gitee.io
- Email: cnwwhai@gmail.com

## TODO
- [x] 插件开源
- [ ] 通过MQTT协议进行串口数据的收发
- [ ] 串口数据通过EMQ转发到后端
- [ ] 适配树莓派4B
- [ ] 适配Windows10
- [ ] 适配更多模块或者开发板