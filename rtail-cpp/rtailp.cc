#include <iostream>
#include <poll.h>
#include <stdio.h>
#include <string>
#include <unistd.h>

#include "zmq.hpp"

using std::string;
using std::cin;
using std::cout;
using std::cerr;

int main (int argc, char *argv[])
{
    if (argc < 3) {
        printf("Usage: %s /path/to/rtaild.sock topic\n", argv[0]);
        return 1;
    }

    const int MAX_LINE = 65535;
    char input_line[MAX_LINE + 1];
    char *line_in;

    try {
        string rtaild_addr (string("ipc://") + argv[1]);
        string topic (argv[2]);

        zmq::context_t ctx (1);
        zmq::socket_t sock (ctx, ZMQ_PUB);
        sock.connect(rtaild_addr.data());

        // line buffering on stdio
        setvbuf (stdin,  NULL, _IOLBF, MAX_LINE);
        setvbuf (stdout, NULL, _IOLBF, MAX_LINE);

        // disable stdio and stream copying mess
        cin.sync_with_stdio(false);

        while ((line_in = fgets(input_line, MAX_LINE, stdin)) != NULL) {
            string line (line_in);

            if (!line.empty()) {
                cout << line;

                // TODO: how to do zero-copy for topic?
                zmq::message_t tpc (topic.size());
                memcpy (tpc.data(), topic.data(), topic.size());
                sock.send(tpc, ZMQ_SNDMORE);

                int n = line.size() - 1;
                zmq::message_t msg (n);
                memcpy (msg.data(), line.substr(0, n).data(), n);
                sock.send(msg);
            }
        }
    } catch(zmq::error_t e) {
        cerr << "error: ZMQ " << e.what() << "\n";
        return 2;
    }

    if (ferror(stdin)) {
        cerr << "error: reading stdin\n";
        return 1;
    }

    return 0;
}
