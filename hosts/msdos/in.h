#ifndef _IN_H
#define _IN_H

struct in_addr {
    unsigned long s_addr;
};

struct sockaddr_in {
    struct in_addr sin_addr;
};

#endif
