import serial
from struct import *
vty = serial.Serial('/dev/ttys017', 115200, 8, 'N', 1)

if vty.is_open:
    while True:
        # ID = 00000000 00000000 00000001 00000000 00000001 data....
        data = vty.read_all()
        if data != b"" and len(data) > 5:
            print("received:", data)
            b_msg_id = data[0:2]
            b_type = data[3]
            b_length = data[4:5]
            msg_id = int.from_bytes(b_msg_id, 'big')
            typo = chr(b_type)
            length = int.from_bytes(b_length, 'big')
            print("msg_id:", msg_id, " Type:", typo, " Length:", length)
            if typo == '0':
                print("PING")
                vty.write(pack('BB', msg_id, 0x01))
                vty.flush()
            if typo == '1':
                print("PING_SUCCESS")


    # B: binary
    #
    # Send ping, Msgid = 1, Type = 0
    # vty.write(pack('BBBB', 0, 0, 1, 0))
    # vty.flush()
    # vty.close()
else:
    print('open failed\n')
