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
#include <boost/assign/list_of.hpp>

using namespace std;
namespace assign = boost::assign;

bool sync_files(const BarnConf& barn_conf) {
  static const auto host_name = get_host_name(); //TODO: make me better

  const auto rsync_target = "rsync://" + barn_conf.barn_rsync_addr
                            + "/" + remote_rsync_namespace  + "/"
                            + barn_conf.service_name + token_separator
                            + barn_conf.category + token_separator
                            + host_name + path_separator;

  const auto rsync_exclusions =
    assign::list_of<string>("--exclude=*.u")
                           ("--exclude=config")
                           ("--exclude=current")
                           ("--exclude=lock")
                           ("--exclude=*~");

  const auto rsync_flags =
    assign::list_of<string>("--times")
                           ("--verbose");

  //TODO reunite rsync_wet_run and rsync_dry_run command generation
  const auto rsync_dry_run =
    assign::list_of<string>("rsync")
                           ("--dry-run")
                           .range(rsync_flags)
                           .range(rsync_exclusions)
                           .range(list_files(barn_conf.rsync_source))
                           (rsync_target);

  const auto rsync_output = run_command("rsync", rsync_dry_run);

  if(rsync_output.first != 0 &&
    rsync_output.first != PARTIAL_TRANSFER &&
    rsync_output.first != PARTIAL_TRANSFER_DUE_VANISHED_SOURCE) {
    cout << "ERROR: Rsync failed to retrieve sync list:" << rsync_output.first << endl;
    cout << rsync_output.second << endl;
    return false;
  }

  auto candidates = get_rsync_candidates(rsync_output.second);
  sort(candidates.begin(), candidates.end());

  if(candidates.size() == 0)
    return false;

  for(const string& el : candidates) {
    cout << "Syncing " + el + " on " + barn_conf.rsync_source << endl;
    const auto file_name = barn_conf.rsync_source + path_separator + el;

   //TODO reunite rsync_wet_run and rsync_dry_run command generation
   const auto rsync_wet_run =
     assign::list_of<string>("rsync")
                            .range(rsync_flags)
                            .range(rsync_exclusions)
                            (file_name)
                            (rsync_target);

    if(run_command("rsync", rsync_wet_run).first != 0) {
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

  if(!contained(candidates, list_file_names(barn_conf.rsync_source)))
    cout << "DANGER: We're producing logs much faster than shipping." << endl;

  return true;
}

bool sleep_it(const BarnConf& barn_conf)  {
  cout << "Sleeping..." << endl;

  try {
    return run_command("inotifywait",
      assign::list_of<string>("inotifywait")
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


