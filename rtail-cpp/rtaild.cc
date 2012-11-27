#include <string>
#include <unistd.h>

#include "zmq.hpp"

using std::string;

int main (int argc, char *argv[])
{
    if (argc < 2) {
        printf("Usage: %s /path/to/sock port\n", argv[0]);
        return 1;
    }

    string sub_addr = string("ipc://") + argv[1];
    string pub_addr = string("tcp://*:") + argv[2];

    zmq::context_t ctx (1);
    zmq::socket_t pub (ctx, ZMQ_PUB);
    zmq::socket_t sub (ctx, ZMQ_SUB);

    sub.bind(sub_addr.data());
    pub.bind(pub_addr.data());

    sub.setsockopt(ZMQ_SUBSCRIBE, "", 0);

    zmq_proxy(sub, pub, NULL);

    sub.close();
    pub.close();
    ctx.close();

    return 0;
}
