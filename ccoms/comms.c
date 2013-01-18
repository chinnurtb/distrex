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

#ifndef SPINWAIT
#define SPINWAIT 3
#endif

#define bool int
#define true 1
#define false 0

struct Connection {
    int SocketFD;
    char* resource;
    bool close;

    struct sockaddr* server;
    pthread_mutex_t* mutex;
    pthread_t* heartbeatthread;

};

bool Lock(struct Connection* cxn);
void Spinlock(struct Connection* cxn);
void Unlock(struct Connection* cxn);
void* heartbeat(void* resource);

int main(int argc, char* argv[]) {
    int sfd;
    int holdtime = 10;
    char* rsrc;

    pthread_t heartbeatthread;
    pthread_mutex_t mutex;
    struct Connection cxn;
    struct sockaddr_in server;

    if (pthread_mutex_init(&mutex, NULL) != 0) {
        perror("Couldn't initialize lock: ");
        exit(1);
    }

    /* get our command line arguments in order. This should probably
     * be using getopt or whatever.
     */
    if (argc < 2) {
        rsrc = "1";
    } else if (argc >= 2) {
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
    cxn.close = false;
    cxn.heartbeatthread = &heartbeatthread;

    /* Lock our resource */
    if (!Lock(&cxn))
        Spinlock(&cxn);
    /* Sleep for a while to simulate other work */
    sleep(holdtime);
    /* Unlock our resource */
    Unlock(&cxn);
    return 0;
}

bool Lock(struct Connection* cxn) {
    /* send our message */
    int len;
    char* buf;
    char recvbuf[BUFSIZE];
    memset(&recvbuf, '\0', sizeof(recvbuf));

    buf = malloc(sizeof("LOCK") + strlen(cxn->resource));
    len = sprintf(buf, "LOCK%s", cxn->resource);

    if (sendto(cxn->SocketFD, buf, len, 0, cxn->server, sizeof(struct sockaddr)) == -1)
        return false;
    if (recvfrom(cxn->SocketFD, recvbuf, BUFSIZE, 0, NULL, NULL) == -1)
        return false;
    if (strcmp(recvbuf, "ok") == 0) {
        if (pthread_create(cxn->heartbeatthread, NULL, heartbeat, (void *) cxn) == 0)
            return true;
        else
            return false;
    } else {
        return false;
    }
}

void Spinlock(struct Connection* cxn) {
    while (!Lock(cxn)) {
        sleep(SPINWAIT);
    }
}

void Unlock(struct Connection* cxn) {
    /* Lock our mutex and set the close to 1 to indicate we're done
     * with the resource.
     */
    pthread_mutex_lock(cxn->mutex);
    cxn->close = true;
    pthread_mutex_unlock(cxn->mutex);
    /* Wait for our thread to join and then return */
    pthread_join(*(cxn->heartbeatthread), NULL);
}

void* heartbeat(void* resource) {
    int len, len2;
    char* buf;
    char* buf2;

    struct Connection* cxn;

    cxn = (struct Connection*) resource;
    if (cxn == NULL)
        return NULL;

    if (cxn->resource == NULL) {
        puts("null");
        return NULL;
    }

    buf = malloc(strlen("BEAT") + strlen(cxn->resource));
    buf2 = malloc(strlen("UNLOCK") + strlen(cxn->resource));

    if (buf == NULL || buf2 == NULL)
        return NULL;

    len = sprintf(buf, "BEAT%s", cxn->resource);
    len2 = sprintf(buf2, "UNLOCK%s", cxn->resource);

    while (1) {
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
