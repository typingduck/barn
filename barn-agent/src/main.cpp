#include <iostream>
#include <string>

#include "barn-agent.h"
#include "sighandle.h"
#include "barn-agent-monitor.h"
#include "params.h"

/*
 * Main loop. Parses command line and based on whether it's in monitor mode,
 * branches into two sub main functions.
 *
 */
int main(int argc, char* argv[]) {
 const BarnConf barn_conf = parse_command_line(argc, argv);

 // Enable a signal handler that given child_pid_global global variable
 // it propagates the SIGTERM signal.
 enable_kill_child_signal_handler();

 /*
  * Barn-agent runs in two modes. Monitor mode listens
  * on a UDP port for statistics coming from barn-agents
  * running in normal mode. Monitor mode is designed to
  * receive, aggregate and publish metrics to ganglia.
  */
 barn_conf.monitor_mode
  ? barn_agent_local_monitor_main(barn_conf)
  : barn_agent_main(barn_conf);
}
