#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>

#define UART "/dev/ttys021"
#define MAX_DATA_LENGTH (65535 + 5)

// | Code (1 Byte) | Body                                                | Name                  |
// | ------------- | --------------------------------------------------- | --------------------- |
// | 0x00          | [MSGID:3, Type:1]                                   | PING                  |
// | 0x01          | [MSGID:3, Type:1]                                   | PING SUCCESS          |
// | 0x02          | [MSGID:3, Type:1 \| Length:2 \| Data:0-65535 Byte ] | DATA SEND             |
// | 0x03          | [MSGID:3, Type:1]                                   | DATA RECEIVED SUCCESS |
// | 0x04          | [MSGID:3, Type:1]                                   | UNKNOWN_PACKET        |
// | 0x05          | [MSGID:3, Type:1]                                   | ERROR                 |
typedef struct __attribute__((packed))
{
    unsigned char id[3];
    unsigned char type[1];
    unsigned char length[2];
    unsigned char data[65535];
} packet;

int uart_init(void)
{
    int fd;
    struct termios oldtio, newtio;
    fd = open((const char *)UART, O_RDWR);
    if (fd < 0)
    {
        printf("uart device: %s open fail\n", UART);
        return -1;
    }
    else
    {
        printf("uart device: %s open success\n", UART);
    }

    if (tcgetattr(fd, &oldtio) != 0)
    {
        printf("uart device: %s tcgetattr fail\n", UART);
        return -1;
    }

    tcgetattr(fd, &oldtio);

    bzero(&newtio, sizeof(newtio));
    newtio.c_cflag = B2400 | CS8 | CLOCAL | CREAD;
    newtio.c_iflag = IGNPAR;
    newtio.c_oflag = 0;
    newtio.c_cc[VTIME] = 0;
    newtio.c_cc[VMIN] = 1;
    tcflush(fd, TCIFLUSH);
    if (tcsetattr(fd, TCSANOW, &newtio) != 0)
    {
        printf("uart device: %s tcsetattr fail\n", UART);
        return -1;
    }

    return fd;
}

int read_data(int uart_fd, unsigned char buffer[])
{
    unsigned char b[MAX_DATA_LENGTH] = {0};
    int eof = read(uart_fd, &b, MAX_DATA_LENGTH);
    if (eof > 0)
    {
        memcpy(buffer, &b, MAX_DATA_LENGTH);
        return 0;
    }
    else
    {
        return 1;
    }
}

int main(int argc, char **argv)
{
    unsigned char buffer[MAX_DATA_LENGTH] = {0};
    int uart_fd = uart_init();
    if (uart_fd < 0)
    {
        return 1;
    }
    while (1)
    {
        read_data(uart_fd, buffer);
        packet p;
        memcpy(&p, buffer, sizeof(packet));
        usleep(100);
    }
    return 0;
}