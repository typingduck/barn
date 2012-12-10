#include <iostream>
#include <string>
#include <unistd.h>

#include "zmq.hpp"

using std::string;
using std::cerr;

int main (int argc, char *argv[])
{
    if (argc < 3) {
        printf("Usage: %s /path/to/sock port\n", argv[0]);
        return 1;
    }

    try {
      string sub_addr (string("ipc://") + argv[1]);
      string pub_addr (string("tcp://*:") + argv[2]);

      zmq::context_t ctx (1);
      zmq::socket_t pub (ctx, ZMQ_PUB);
      zmq::socket_t sub (ctx, ZMQ_SUB);

      sub.bind(sub_addr.data());
      pub.bind(pub_addr.data());

      sub.setsockopt(ZMQ_SUBSCRIBE, "", 0);

      zmq_proxy(sub, pub, NULL);
    } catch (zmq::error_t e) {
        cerr << "error: ZMQ " << e.what() << "\n";
        return 2;
    }

    return 0;
}
