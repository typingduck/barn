#include <iostream>
#include <vector>
#include <algorithm>
#include "barn-agent.h"
#include "process.h"
#include "rsync.h"
#include "files.h"
#include "helpers.h"

using namespace std;
using namespace boost::assign;
using namespace boost;

Validation<FileNameList> query_candidates(const BarnConf& barn_conf) {
  const auto rsync_target = get_rsync_target(barn_conf);

  const auto rsync_dry_run =
    list_of<string>(rsync_executable_name)
                   (rsync_dry_run_flag)
                   .range(rsync_flags)
                   .range(rsync_exclusions)
                   .range(list_files(barn_conf.rsync_source))
                   (rsync_target);

  const auto rsync_output = run_command("rsync", rsync_dry_run);

  if(rsync_output.first != 0 && rsync_output.first != PARTIAL_TRANSFER &&
     rsync_output.first != PARTIAL_TRANSFER_DUE_VANISHED_SOURCE)
  return BarnError(string("Failed to retrieve sync list: ") + rsync_output.second);

  return get_rsync_candidates(rsync_output.second);
}

Validation<ShipStatistics>
ship_candidates(vector<string> candidates, const BarnConf& barn_conf) {

  const int candidates_size = candidates.size();

  if(!candidates_size) return ShipStatistics(0, 0, 0);

  sort(candidates.begin(), candidates.end());

  const auto rsync_target = get_rsync_target(barn_conf);
  auto num_lost_during_ship(0);

  for(const string& el : candidates) {
    cout << "Syncing " + el + " on " + barn_conf.rsync_source << endl;
    const auto file_name = barn_conf.rsync_source + path_separator + el;

    const auto rsync_wet_run = list_of<string>(rsync_executable_name)
                                              .range(rsync_flags)
                                              .range(rsync_exclusions)
                                              (file_name)
                                              (rsync_target);

    if(run_command("rsync", rsync_wet_run).first != 0) {
      cout << "ERROR: Rsync failed to transfer a log file." << endl;

      if(!file_exists(file_name)) {
        cout << "FATAL: Couldn't ship log since it got rotated in the meantime" << endl;
        num_lost_during_ship += 1;
      } else
        return BarnError("ERROR: Coudln't ship log possibly due to a network error");
    }
  }

  int num_rotated_during_ship(0);

  if((num_rotated_during_ship =
      count_missing(candidates, list_file_names(barn_conf.rsync_source))) != 0)
    cout << "DANGER: We're producing logs much faster than shipping." << endl;

  return ShipStatistics(candidates_size
                      , num_rotated_during_ship
                      , num_lost_during_ship);
}

bool sleep_it(const BarnConf& barn_conf)  {
  cout << "Sleeping..." << endl;

  try {
    return run_command("inotifywait",
      list_of<string>("inotifywait")
                             ("--exclude")
                             ("'\\.u'")
                             ("--exclude")
                             ("'lock'")
                             ("--exclude")
                             ("'current'")
                             ("--timeout")
                             ("3600")
                             ("-q")
                             ("-e")
                             ("moved_to")
                             (barn_conf.rsync_source + "/")).first;

  } catch (const fs_error& ex) {
    cout << "You appear not having inotifywait, sleeping for 3 seconds."
         << ex.what() << endl;
    sleep(3);
    return true;
  }
}

