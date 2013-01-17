#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>


int main(int argc, char* argv[]) {
	int sfd;
	struct sockaddr_in server;

	char* buf = "LOCK1";

	/* Create a fd for our socket */
	if ((sfd = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
		perror("Error creating socket");
		exit(1);
	}

	/* Create our server addr */
	memset(&server, 0, sizeof(struct sockaddr_in));
	server.sin_family = AF_INET;
	server.sin_port = htons(8789);
	server.sin_addr.s_addr = inet_addr("127.0.0.1");

	/* send our message */
	if (sendto(sfd, buf, 6, 0, (struct sockaddr*) &server, sizeof(struct sockaddr)) == -1)
		perror("Send to didn't succeed");


	/*
	  TODO

	  So far this file is a placeholder to remind me to implement the
	  distrex api in a C client.

	  We need:-

	  * Possibly a type to represent an async'd heartbeat
	  * Wrap the nasty socket stuff up in a nice local api, maybe I can
		borrow the nice code from the Linux Programming Interface Book
		providing I can wrap it for Windows, too.
	*/
	return 0;
}
