#include <iostream>
#include <iostream>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

struct BarnConf {
  string barn_rsync_addr;
  string rsync_source;
  string service_name;
  string category;
};

bool sync_files(const BarnConf& barn_conf);
bool sleep_it(const BarnConf& barn_conf);

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

int main(int argc, char* argv[]) {
  const BarnConf barn_conf = parse_command_line(argc, argv);
  while(true) sync_files(barn_conf) && sleep_it(barn_conf);
}

const pair<int, string> run_command(const string& cmd) {
  const int MAX_BUFFER = 255;
  char buffer[MAX_BUFFER];
  string stdout;
  FILE* stream = popen(("exec " + cmd).c_str(), "r");
  while ( fgets(buffer, MAX_BUFFER, stream) != NULL ) stdout.append(buffer);
  int exit_code = pclose(stream);
  return make_pair(exit_code, stdout);
}

const vector<string> get_candidates(string rsync_output) {
  std::stringstream stream(rsync_output);
  vector<string> candidates;
  while(!stream.eof()) {
    std::string line;
    getline(stream, line, '\n');
    if(line[0] == '@') {
      candidates.push_back(line);
    }
  }
  return candidates;
}

bool sync_files(const BarnConf& barn_conf) {
  const auto host_name = "localhost";
  const auto space = " ";
  const auto token_separator = "@";
  const auto path_separator = "/";
  const string rsync_flags = "-c --verbose";
  const auto rsync_exclusions = "--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~";
  const auto rsync_initials = rsync_flags + space + rsync_exclusions;

  const auto rsync_target = "rsync://" + barn_conf.barn_rsync_addr
                            + "/barn_logs/"
                            + barn_conf.service_name + token_separator
                            + barn_conf.category + token_separator
                            + host_name + path_separator;

  const auto rsync_dry_run = "rsync --dry-run "
                             + rsync_initials + space
                             + barn_conf.rsync_source + "/*" + space
                             + rsync_target;

  const auto rsync_output = run_command(rsync_dry_run);

  if(rsync_output.first != 0) {
    cout << "rsync returned non-zero..." << endl;
    return false;
  }

  auto candidates = get_candidates(rsync_output.second);
  sort(candidates.begin(), candidates.end());

  for(string& str : candidates)
    cout << str << endl;

  for(string& str : candidates) {
    cout << "Candidate on " + barn_conf.rsync_source + " is " + str << endl;
    string rsync_command = "rsync "
                         + rsync_initials + space
                         + barn_conf.rsync_source + path_separator + str + space
                         + rsync_target;
    run_command(rsync_command).second;
  }

  return true;
}

bool sleep_it(const BarnConf& barn_conf)  {
  cout << "Sleeping..." << endl;
  run_command("inotifywait $INOTIFY_EXCLUSIONS --timeout 3600 -q -e close_write " + barn_conf.rsync_source + "/");
}

