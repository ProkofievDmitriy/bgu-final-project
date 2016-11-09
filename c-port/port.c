/* port.c */
#include "port.h"


int main() {

  byte buf[100],  buffer[4];
  configure_terminal();
  init_file();
  //set_nonblock_flag(0,1);
  //printf("1\n");
	memset(buf,0,100);
	writeToFile("1\n");
	while(1){
		configure_input_select();
		wait_for_input();
		handle_input();
	}//while(1)  
}//main






/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			Input Related Functions		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*this function is executed only if wait_for_input() returnd, which means input is available.
**it check which input is ready, and act as follows:
** input from stdin - Erlang app send a Packet -> send it through Terminal
**input from terminal_fd - Modem sent a Packet -> send it to Erlang App through stdin*/ 
void handle_input(){
			//writeToFile("at handle input\n");
	  if (FD_ISSET(0, &readfs))         /* input from source 1 available - Erlang App is sending packet  */
            {writeToFile("1c1\n"); handle_input_from_erlang(); }
      if (FD_ISSET(terminal_fd, &readfs))         /* input from source 2 available - Modem is sending packet */
        		{ writeToFile("1c2\n"); handle_input_from_modem(); }

} 

void configure_input_select(){
	   /*select() configuration*/
   	FD_ZERO(&readfs);
    FD_SET(0, &readfs);  /* set testing for source 1 - sdtin */
    FD_SET(terminal_fd, &readfs);  /* set testing for source 2 - terminal_fd */
}
    
void wait_for_input(){
	//printf("1a\n");
	writeToFile("1a\n");
	select(terminal_fd + 1, &readfs, NULL, NULL, NULL);
	//printf("1b\n");
	writeToFile("1b\n");
}    

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			Erlang Related Functions		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ack(byte *buffer, int len){
	send_to_erlang(buffer,len);
}

void handle_input_from_erlang(){
	int rec_len, sent_len;
	byte buffer[100];
	byte fail[] = {'f','a','i', 'l'};
	byte success[] = {'s','u','c','c','e','s','s'}; 	
	
	/*receive packet from modem*/
	rec_len = get_erlang_packet(buffer);
	
	/*if received packet illegal*/
	if(rec_len < 0 ) {/*ack(fail, 4);*/ return;}
	
	/*send modem the given packet*/
	sent_len = send_to_Modem(buffer, rec_len);
	
	/*if number of sent bytes are the same we required send ack to erlang app*/
	/*if( rec_len == sent_len && rec_len>0)	
		ack(success,7);
	else
		ack(fail,4);*/
}

int get_erlang_packet(byte *buf){
 	byte type, arg, res, i=0, counter=0, a;
	char m[50];
	byte buffer[10];
	int type_show=0, arg_show=0;	
	writeToFile("2a\n");
	while(1){
		/*read stdin stream into buffer (get erlang packet)*/
		if(read_cmd(buffer) < 1) 	//if reading stdin return value is <0 stop trying reading it
			{continue;}
   		type = buffer[0];		//get header
   		arg = buffer[1];		//get byte
   		type_show = type & 0xFF;
   		arg_show = arg & 0xFF;
		/*if header is of first byte of packet*/
   		if (type == START_BYTE) {
		/*if packet is of bad format (no CHANNEL byte (1/2/3) at byte indexed 0) - return error for getting erlang packet*/
   			if(arg !=CHANNEL_RF && arg!=CHANNEL_PLC && arg!=CHANNEL_RF_AND_PLC)  {writeToFile("arg!=1,2,3\n");return ERROR;}
		/*else - place received byte at buffer*/
    		a = arg & 0xFF;
     		buf[i] = a;
      		i++; counter++;

		/*if header is of  one of the "middle" bytes of packet*/
    	} else 
    	if (type == REG_BYTE) {
		/*if we got "middle" byte before start byte - return error for getting erlang packet*/
    		if(counter ==0) /*bad byte received*/ {writeToFile("teyp==1 but counter==0\n");return ERROR;}
			/*else - place received byte at buffer*/
    		a = arg & 0xFF;
      		buf[i] = a;
      		i++;counter++;
		/*if header is of  last byte of packet*/
   		} else 
   		if (type == END_BYTE) {
			/*if we got end byte before start byte - return error for getting erlang packet*/
   			if(counter ==0) /*bad byte received*/ {writeToFile("teyp==255 but counter==0\n");return ERROR;}
			/*else - place received byte at buffer*/
    			a = arg & 0xFF;
	    		buf[i] = a;
			i=0;counter++;
			sprintf(m, "got last byte. len is:%d, whole buff is:\n", counter);
     			//writeToFile(m);
      			//writeToFile2(buf, counter);
     			//writeToFile("\n");
      			writeToFile("3\n");
     			return counter;
    	}//if(END_BYTE)
 	 }//while
}//get_erlang_packet

int send_to_erlang(byte* buffer, int len){
		writeToFile("sent it to erl\n");
		writeToFile2(buffer, len);
		writeToFile("\n");
		return write_cmd(buffer, len);
	}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			Modem Related Functions		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void handle_input_from_modem(){
	int i=0, counter=0, short_byte;
	byte buffer[PACKET_SIZE], r_byte;
	char msg[40];
	char m[100]; 
	short len = 1;
	int int_len = len & 0xff;
	memset(buffer, 0, PACKET_SIZE);
	writeToFile("1c\n");	
	len = read(terminal_fd,buffer,PACKET_SIZE);
	sprintf(m, "receiving Modem Packet: len is:%hu , whole buff is:\n",  len);
    writeToFile(m);
	writeToFile("1d\n");
	if(!packet_ok(buffer, len))
		{ writeToFile("packet's payload illegal\n"); writeToFile2(buffer, len); writeToFile("\n");return;}
	
	//if(len == PACKET_SIZE){
	writeToFile("send to erlang\n");
	send_to_erlang(buffer,len);
	//}
	if(len == 0){
		writeToFile("len=0\n"); return;
	}
	//if(len<PACKET_SIZE && len>0)
	//	writeToFile("len>0 len <PACKET_SIZE\n");
		
		
	writeToFile2(buffer, len);
	writeToFile("\n");
	
	//printf("got data:\n");
	//show_buffer(buffer, counter);
	//send to erlang

		
}

int send_to_Modem(byte * buffer, short len){
	int i=0;
	char m[100];
	int int_len = len & 0xff;
	unsigned int a = len % 20, b = len/20;
	short packet_size;
	if(a==0)  {packet_size= b*20;}
	else {packet_size= (b+1)*20;}
	if(len>40) {packet_size=66;}
	if(len==0) {return;}
	sprintf(m, "sending Modem a Packet: len is:%d packet_size is:%d, whole buff is:\n", packet_size, int_len);
    writeToFile(m);
	writeToFile2(buffer,packet_size);
	writeToFile("\n#1\n");
	/*for(i=0; i<len;i++){
		write(terminal_fd, &buffer[i], 1);
		writeToFile("#");
		}
		writeToFile("\n\n");*/
	return write(terminal_fd, buffer, packet_size);


}

void configure_terminal(){
	int rc;
	
	/*Terminal configuration*/
	terminal_fd = open(MODEMDEVICE, O_RDWR | O_NOCTTY ); 
	
 /* Check if the file descriptor is pointing to a TTY device or not*/
 	if(!isatty(terminal_fd)) {  }
	if (terminal_fd <0) {perror(MODEMDEVICE);  }
	
	
	if((rc = tcgetattr(terminal_fd, &newtio)) < 0){
        fprintf(stderr, "failed to get attr: %d, %s\n", terminal_fd, strerror(errno));
        exit(EXIT_FAILURE);
    }

    // Set the baud rates to 
    cfsetispeed(&newtio, BAUDRATE);

    // Set the baud rates to 
    cfsetospeed(&newtio, BAUDRATE);

    cfmakeraw(&newtio);
    newtio.c_cflag |= (CLOCAL | CREAD);   // Enable the receiver and set local mode
    newtio.c_cflag &= ~CSTOPB;            // 1 stop bit
    newtio.c_cflag &= ~CRTSCTS;           // Disable hardware flow control
    newtio.c_cc[VMIN]  = 0;
    newtio.c_cc[VTIME] = 1;

    // Set the new attributes
    if((rc = tcsetattr(terminal_fd, TCSANOW, &newtio)) < 0){
        fprintf(stderr, "failed to set attr: %d, %s\n", terminal_fd, strerror(errno));
        exit(EXIT_FAILURE);
    }


}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			Other		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void show_buffer(byte *buffer, short len){
	int i=0, n=0; 
	int k=0;
	//printf("len is:%d\n", len);
	for(i;i<len; i++){
		n = buffer[i];
		n = n & 0xFF;
		printf("(%d)%d_",i, n);
		if(k>10) { k=0; }
		}
	printf("\n");

}

/*check if the packet given is at goot format (first byte is CHANNEL), and if the payload (bytes 3-...) are equals (bad packet). 
return 1 if ok, 0 else*/
int packet_ok(byte* buffer, int len){
		if(buffer[0] != CHANNEL_RF && buffer[0] !=CHANNEL_PLC)
			return FALSE;
		if(buffer[0] == buffer[1]) return FALSE;
		//if(buffer[len-1] == buffer[2] && buffer[5] == buffer[6])
		//	return FALSE;
		if(buffer[10] == buffer[11] &&  buffer[20] == buffer[21] && buffer[30] == buffer[31]) return FALSE;
		return TRUE;
}

/* Set the O_NONBLOCK flag of desc if value is nonzero,
   or clear the flag if value is 0.
   Return 0 on success, or -1 on error with errno set. */
int set_nonblock_flag(int desc, int value)
{
  int oldflags = fcntl (desc, F_GETFL, 0);
  /* If reading the flags failed, return error indication now. */
  if (oldflags == -1)
    return -1;
  /* Set just the flag we want to set. */
  if (value != 0)
    oldflags |= O_NONBLOCK;                                                       
  else
    oldflags &= ~O_NONBLOCK;
  /* Store modified flag word in the descriptor. */
  return fcntl (desc, F_SETFL, oldflags);
}
    

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////	Erlang <--> C Pipeliling		///////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**FUNCTION FOR PIPLINING WITH ERLANG THROUGH STDIN, STDOUT
** Source - erlang officel documents example
**/
int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}







/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			File Handler		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/**this function write to file (log) the last buffer sent.
**Parameter: FILE* src - a pointer for a file descriptor to handle file. int type - first, last or middle byte of info,
** unsigned char byte - the byte to write to file
**/
void writeToFile(char* str){
	if ( (src = fopen(LOG_FILE_PATH, "a")  ) < 0) {exit(-1);}
	if( (fputs(str, src) ) <0 ) {exit(-11);}
	fclose(src);
}


void writeToFile2( byte* buf, int len){
	int i=0,l;
	char s[10];
	for(i=0;i<len;i++){
		l = (buf[i] & 0xFF);
		sprintf(s, "(%d)%d_",i, l);
		writeToFile(s);
		memset(s,0,10);
	}
}


void init_file(){
	if ( (src = fopen(LOG_FILE_PATH, "w+")  ) < 0) {exit(-1);}
	fclose(src);
	writeToFile(   "init log file\n");
	//fclose(src);
}



