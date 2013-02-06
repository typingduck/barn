#include <iostream>

#include "params.h"

using namespace std;

const BarnConf parse_command_line(int argc, char* argv[]) {
  BarnConf conf;

  if(argc != 5) {
    cout << "Usage: $0 RSYNC_HOST:RSYNC_PORT RSYNC_SOURCE SERVICE_NAME CATEGORY" << endl;
    exit(1);
  }

  conf.barn_rsync_addr = string(argv[1]);
  conf.rsync_source = string(argv[2]);
  conf.service_name = string(argv[3]);
  conf.category = string(argv[4]);

  return conf;
}

