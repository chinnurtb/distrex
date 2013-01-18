#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef BUFSIZE
#define BUFSIZE 1024
#endif

struct Connection {
    int SocketFD;
    char* resource;
    struct sockaddr* server;
};

void* heartbeat(void* resource) {
    int len;
    char* buf;
    struct Connection* cxn;

    cxn = (struct Connection*) resource;
    if (cxn == NULL)
        return NULL;

    buf = malloc(strlen("BEAT") + strlen(cxn->resource));
    if (buf == NULL)
        return NULL;

    len = sprintf(buf, "BEAT%s", cxn->resource);
    while (1) {
        if (sendto(cxn->SocketFD, buf, len, 0, cxn->server, sizeof(struct sockaddr)) == -1) {
            free(buf);
            return NULL;
        }
        sleep(3);
    }
}

int main(int argc, char* argv[]) {
    int sfd;
    char* buf;
    struct Connection cxn;
    struct sockaddr_in server;
    if (argc < 2)
        buf = "LOCK1";
    else
        buf = argv[1];


    // Create and zero out our buffer.
    char recvbuf[BUFSIZE];
    memset(&recvbuf, '\0', sizeof(recvbuf));

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

    /* Create our connection */
    cxn.SocketFD = sfd;
    cxn.server = (struct sockaddr*) &server;
    cxn.resource = "1";

    /* send our message */
    if (sendto(cxn.SocketFD, buf, strlen(buf), 0, cxn.server, sizeof(struct sockaddr)) == -1)
        perror("Sending failed: ");
    if (recvfrom(sfd, recvbuf, BUFSIZE, 0, NULL, NULL) == -1)
        perror("Receiving failed: ");
    else {
        /* This should be threaded */
        if (strcmp(recvbuf, "ok") == 0) {
            heartbeat((void *) &cxn);
        }
    }

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
