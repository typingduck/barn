#include "rsync.h"
#include <sstream>
#include <vector>

using namespace std;

const vector<string> get_rsync_candidates(string rsync_output) {
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

