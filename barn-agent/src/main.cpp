#include "barn-agent.h"

#include <boost/assign/list_of.hpp>
#include <iostream>
#include <string>
#include "sighandle.h"
#include "process.h"

using namespace std;

int main(int argc, char* argv[]) {

  install_signal_handler();

  const BarnConf barn_conf = parse_command_line(argc, argv); //Exit if invalid
  while(true) sync_files(barn_conf) || sleep_it(barn_conf);
}
