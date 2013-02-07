#include <iostream>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>
#include "barn-agent.h"
#include "process.h"
#include "rsync.h"
#include "files.h"
#include "helpers.h"
#include <unistd.h>

using namespace std;

bool sync_files(const BarnConf& barn_conf) {
  static const auto host_name = get_host_name(); //TODO: make me better
  const auto rsync_initials = rsync_flags + space + rsync_exclusions;

  const auto rsync_target = "rsync://" + barn_conf.barn_rsync_addr
                            + "/" + remote_rsync_namespace  + "/"
                            + barn_conf.service_name + token_separator
                            + barn_conf.category + token_separator
                            + host_name + path_separator;

  const auto rsync_dry_run = "rsync --dry-run "
                             + rsync_initials + space
                             + barn_conf.rsync_source + "/*" +
                             + space + rsync_target + " 2>/dev/null";

  const auto rsync_output = run_command(rsync_dry_run);

  if(rsync_output.first != 0 && rsync_output.first != 5888) {
    cout << "ERROR: Rsync failed to retrieve sync list:" << rsync_output.first << endl;
    return false;
  }

  auto candidates = get_rsync_candidates(rsync_output.second);
  sort(candidates.begin(), candidates.end());

  if(candidates.size() == 0)
    return false;

  for(vector<string>::const_iterator it = candidates.begin(); it < candidates.end(); ++ it) {
    cout << "Syncing " + *it + " on " + barn_conf.rsync_source << endl;
    const auto file_name = barn_conf.rsync_source + path_separator + *it;

    string rsync_command = "rsync "
                         + rsync_initials + space
                         + file_name + space
                         + rsync_target;

    if(run_command(rsync_command).first != 0) {
      cout << "ERROR: Rsync failed to transfer a log file." << endl;
      if(!file_exists(file_name))
        cout << "ERROR: Couldn't ship log since it got rotated in the meantime" << endl;
      else  {
        cout << "ERROR: Coudln't ship log possibly due to a network error" << endl;
        sleep(1);
        return true;
      }
    }
  }

  if(!contained(candidates, list_files(barn_conf.rsync_source)))
    cout << "DANGER: We're producing logs much faster than shipping." << endl;

  return true;
}

bool sleep_it(const BarnConf& barn_conf)  {
  cout << "Sleeping..." << endl;
  run_command("inotifywait " + inotify_exclusions + " --timeout 3600 -q -e moved_to " + barn_conf.rsync_source + "/");
}


