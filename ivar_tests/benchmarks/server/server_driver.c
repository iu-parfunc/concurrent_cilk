// This is the client computation that drives the server test in this directory.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 


static const int port = 5002;
char* host = "localhost";

int numconn = 4;
int units   = 5; // Number of communication rounds for each client/server pairing.

void error(const char *msg)
{
    perror(msg);
    exit(0);
}

int main(int argc, char **argv) {
    printf("==== server_test_driver: simple client to hammer the server (on port %d)\n", port);

    //    int sockfd;
    struct sockaddr_in serv_addr;
    struct hostent *server;
    /*
    if (argc < 3) {
       fprintf(stderr,"usage %s connections rounds\n", argv[0]);
       exit(0);
    }
    */
    //    portno = atoi(argv[2]);
    server = gethostbyname(host);
    if (server == NULL) {
        fprintf(stderr,"ERROR, no such host\n");
        exit(0);
    }
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *)server->h_addr, 
         (char *)&serv_addr.sin_addr.s_addr,
         server->h_length);
    serv_addr.sin_port = htons(port);
   
    int i,r;
    char buffer[256];    
    int* sockfds = malloc(sizeof(int) * numconn);
    // In this simple test we don't do any real compute on the client side.
    for(i=0; i<numconn; i++) 
    {
        printf("Socket connection # %d\n",i);
        sockfds[i] = socket(AF_INET, SOCK_STREAM, 0);
        if (sockfds[i] < 0) 
            error("ERROR opening socket");
        if (connect(sockfds[i],(struct sockaddr *) &serv_addr,sizeof(serv_addr)) < 0) 
           error("ERROR connecting");

        /*
        for(r=0; r<units; r++) {
          buffer[0] = (char)(i);
          write(sockfds[i],buffer,1);
        }
        */
    }

    for(r=0; r<units; r++)
      for(i=0; i<numconn; i++) {
          buffer[0] = (char)(i);
          write(sockfds[i],buffer,1);
      }

    printf("Done writing %d messages.\n", units*numconn);

    /*
    printf("Please enter the message: ");
    bzero(buffer,256);
    fgets(buffer,255,stdin);
    n = write(sockfd,buffer,strlen(buffer));
    if (n < 0) 
         error("ERROR writing to socket");
    bzero(buffer,256);
    n = read(sockfd,buffer,255);
    if (n < 0) 
         error("ERROR reading from socket");
    printf("%s\n",buffer);
    */

    // close(sockfd);
    for(i=0; i<numconn; i++) close(sockfds[i]);
    return 0;
}
