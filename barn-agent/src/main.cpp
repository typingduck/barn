#include <boost/assign/list_of.hpp>
#include <boost/variant.hpp>

#include <iostream>
#include <string>

#include "barn-agent.h"
#include "sighandle.h"
#include "process.h"
#include "monitor_main.h"
#include "localreport.h"

using namespace std;
using namespace boost;

void handle_failure_in_sync_round(BarnConf barn_conf, BarnError error);
void execute_single_sync_round(BarnConf barn_conf, FileNameList file_name_list);
void barn_agent_main(const BarnConf& barn_conf);

int main(int argc, char* argv[]) {
 const BarnConf barn_conf = parse_command_line(argc, argv);

 install_signal_handler();

 barn_conf.monitor_mode ?
    barn_agent_local_monitor_main(barn_conf)
 :
    barn_agent_main(barn_conf);
}

void barn_agent_main(const BarnConf& barn_conf) {
  while(true) {
    fold(query_candidates(barn_conf),
      [&](FileNameList file_name_list) { execute_single_sync_round(barn_conf, file_name_list); },
      [&](BarnError error) { handle_failure_in_sync_round(barn_conf, error); }
    );
  }
}

void handle_failure_in_sync_round(const BarnConf barn_conf, BarnError error) {
  cout << "Error:" << error << endl;

  send_report(barn_conf.monitor_port,
    Report(barn_conf.service_name, barn_conf.category, FailedToGetSyncList, 1));

  sleep_it(barn_conf);
}

void execute_single_sync_round(const BarnConf barn_conf, FileNameList file_name_list) {
  send_report(barn_conf.monitor_port,
    Report(barn_conf.service_name, barn_conf.category, FilesToShip,
    file_name_list.size()));

  fold(ship_candidates(file_name_list, barn_conf),
    [&](ShipStatistics ship_statistics) {

      send_report(barn_conf.monitor_port,
        Report(barn_conf.service_name, barn_conf.category, RotatedDuringShip,
          ship_statistics.num_rotated_during_ship));

      send_report(barn_conf.monitor_port,
        Report(barn_conf.service_name, barn_conf.category, LostDuringShip,
          ship_statistics.num_lost_during_ship));

      ship_statistics.num_shipped || sleep_it(barn_conf);
    },
    [&](BarnError error) {
      cout << "Error:" << error << endl;
      sleep_it(barn_conf);
    }
  );
}

