#include <stdio.h>
#include <string.h>   //strlen
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>   //close
#include <arpa/inet.h>    //close
#include <sys/types.h>
#include <sys/time.h> //FD_SET, FD_ISSET, FD_ZERO macros
#include <termios.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <stdint.h>     // C99 fixed data types


#define BAUDRATE B38400
//#define MODEMDEVICE "/dev/ttyUSB0"
#define MODEMDEVICE "/dev/ttyMFD1"  
//#define MODEMDEVICE "/dev/ttyACM0"  

#define TRUE   1
#define FALSE  0
#define PACKET_SIZE	66

#define OK			1
#define ERROR			-1

#define FILE_ERROR		-10
#define MEMORY_ERROR		-11
#define UART_ERROR		-12
	
#define LOG_FILE_PATH 				"log.txt"

#define START_BYTE		0
#define END_BYTE		255
#define REG_BYTE		1

#define CHANNEL_RF		1
#define CHANNEL_PLC		2
#define CHANNEL_RF_AND_PLC	3

typedef unsigned char byte;

/*Globals*/
int terminal_fd = -1;
struct termios oldtio,newtio;
fd_set readfs;
FILE* src;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

void show_buffer(byte *buffer, short len);

int	set_nonblock_flag(int desc, int value);

void init_file();
void writeToFile2( byte* str, int len);
void writeToFile(char* str);

void configure_input_select();
void handle_input();
void wait_for_input();

void handle_input_from_erlang();
int get_erlang_packet(byte* buf);
int send_to_erlang(byte* buffer, int len);

void configure_terminal();
void handle_input_from_modem();
int send_to_Modem(byte * buffer, short len);
