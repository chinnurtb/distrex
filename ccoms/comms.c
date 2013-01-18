#include <arpa/inet.h>
#include <assert.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef BUFSIZE
#define BUFSIZE 1024
#endif

struct Connection {
    int SocketFD;
    char* resource;
    struct sockaddr* server;
    pthread_mutex_t* mutex;
    int close;
};

void* heartbeat(void* resource);

int main(int argc, char* argv[]) {
    int sfd;
    int rc;
    int holdtime = 10;

    char* buf;
    char* rsrc;

    pthread_t heartbeatthread;
    pthread_mutex_t mutex;

    if (pthread_mutex_init(&mutex, NULL) != 0) {
        perror("Couldn't initialize lock: ");
        exit(1);
    }

    struct Connection cxn;
    struct sockaddr_in server;

    /* get our command line arguments in order. This should probably
     * be using getopt or whatever.
     */
    if (argc < 2) {
        buf = "LOCK1";
        rsrc = "1";
    } else if (argc >= 2) {
        buf = malloc(strlen("LOCK") + strlen(argv[1]));
        sprintf(buf, "LOCK%s", argv[1]);
        rsrc = argv[1];
    }

    if (argc >= 3)
        holdtime = atoi(argv[2]);

    /* Create and zero out our buffer. */
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
    cxn.resource = rsrc;
    cxn.mutex = &mutex;
    cxn.close = 0;

    /* send our message */
    if (sendto(cxn.SocketFD, buf, strlen(buf), 0, cxn.server, sizeof(struct sockaddr)) == -1)
        perror("Sending failed: ");
    if (recvfrom(sfd, recvbuf, BUFSIZE, 0, NULL, NULL) == -1)
        perror("Receiving failed: ");
    else {
        if (strcmp(recvbuf, "ok") == 0) {
            rc = pthread_create(&heartbeatthread, NULL,  heartbeat, (void *) &cxn);
            assert(rc == 0);
        } else {
            puts("Did not acquire the lock");
            exit(1);
        }
    }

    /* Sleep for a while to simulate other work */
    sleep(holdtime);

    /* Lock our mutex and set the close to 1 to indicate we're done
     * with the resource.
     */
    pthread_mutex_lock(cxn.mutex);
    cxn.close = 1;
    pthread_mutex_unlock(cxn.mutex);

    /* Wait for our thread to join and then return */
    pthread_join(heartbeatthread, NULL);
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

void* heartbeat(void* resource) {
    int len, len2;
    char* buf;
    char* buf2;

    struct Connection* cxn;

    cxn = (struct Connection*) resource;
    if (cxn == NULL)
        return NULL;

    buf = malloc(strlen("BEAT") + strlen(cxn->resource));
    buf2 = malloc(strlen("UNLOCK") + strlen(cxn->resource));

    if (buf == NULL || buf2 == NULL)
        return NULL;

    len = sprintf(buf, "BEAT%s", cxn->resource);
    len2 = sprintf(buf2, "UNLOCK%s", cxn->resource);

    while (1) {
        puts("heartbeat");
        pthread_mutex_lock(cxn->mutex);
        if (cxn->close) {
            if (sendto(cxn->SocketFD, buf2, len2, 0, cxn->server, sizeof(struct sockaddr)) == -1) {
                free(buf);
                return NULL;
            }
            free(buf);
            return NULL;
        }
        pthread_mutex_unlock(cxn->mutex);
        if (sendto(cxn->SocketFD, buf, len, 0, cxn->server, sizeof(struct sockaddr)) == -1) {
            free(buf);
            return NULL;
        }
        sleep(3);
    }
}
