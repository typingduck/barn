#include <iostream>
#include <poll.h>
#include <stdio.h>
#include <string>
#include <unistd.h>

#include "zmq.hpp"

using std::string;
using std::cin;
using std::cout;

int main (int argc, char *argv[])
{
    if (argc < 2) {
        printf("Usage: %s /path/to/rtaild.sock topic\n", argv[0]);
        return 1;
    }

    string rtaild_addr = string("ipc://") + argv[1];
    string topic = argv[2];

    zmq::context_t ctx (1);
    zmq::socket_t sock (ctx, ZMQ_PUB);
    sock.connect(rtaild_addr.data());

    pollfd pfd;
    pfd.fd = 0;
    pfd.events = POLLIN;

    // line buffering on stdio
    setvbuf(stdin, NULL, _IOLBF, 1024);
    setvbuf(stdout, NULL, _IOLBF, 1024);

    string line;
    while (poll(&pfd, 1, -1) && cin.good()) {
        cin.clear();
        getline(cin, line);

        if (!line.empty()) {
            cout << line << "\n";

            zmq::message_t tpc (topic.size());
            memcpy (tpc.data(), topic.data(), topic.size());
            sock.send(tpc, ZMQ_SNDMORE);

            zmq::message_t msg (line.size());
            memcpy (msg.data(), line.data(), line.size());
            sock.send(msg);
        }

        line.clear();
    }

    sock.close();
    ctx.close();

    return 0;
}
