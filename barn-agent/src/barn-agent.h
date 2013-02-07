#include <vector>
#include <string>

#ifndef BARN_AGENT_H
#define BARN_AGENT_H

const auto space = " ";
const auto token_separator = "@";
const auto path_separator = "/";

//TODO Make us configurable
const std::string rsync_flags = "-t --verbose";
const auto rsync_exclusions = "--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~";
const auto remote_rsync_namespace = "barn_logs";
const std::string inotify_exclusions = "--exclude '\\.u' --exclude 'lock' --exclude 'current'";

struct BarnConf {
  std::string barn_rsync_addr;
  std::string rsync_source;
  std::string service_name;
  std::string category;
};

bool sync_files(const BarnConf& barn_conf);
bool sleep_it(const BarnConf& barn_conf);
const BarnConf parse_command_line(int argc, char* argv[]);

#endif
