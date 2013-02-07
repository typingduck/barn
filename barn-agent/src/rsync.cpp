#include "rsync.h"
#include "helpers.h"
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

const vector<string> choose_earliest_subset(vector<string> file_names) {
  sort(file_names.begin(), file_names.end());

  const int last = file_names.size() - 2;
  const int middle = (file_names.size() - 2) / 2;

  return vector<string>(&file_names[0], &file_names[middle]);
}

const vector<string> get_rsync_candidates(string rsync_output) {
  const auto lines = split(rsync_output, '\n');
  vector<string> svlogd_files;

  for(vector<string>::const_iterator it = lines.begin(); it < lines.end(); ++it) {
    if((*it)[0] == '@') {
      svlogd_files.push_back(*it);
    }
  }

  return svlogd_files;
}

