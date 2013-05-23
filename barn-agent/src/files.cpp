#include <iostream>
#include <vector>
#include "files.h"
#include "process.h"
#include "helpers.h"
#include <unistd.h>
#include <boost/filesystem.hpp>

using namespace std;
namespace fs = boost::filesystem;

vector<string> list_files(string path_) {
  const fs::path path(path_);
  const fs::directory_iterator end;
  vector<string> file_names;

  for(fs::directory_iterator it(path); it != end ; ++it)
    file_names.push_back(it->path().string());

  return file_names;
}

//TODO don't duplicate me!
vector<string> list_file_names(string path_) {
  const fs::path path(path_);
  const fs::directory_iterator end;
  vector<string> file_names;

  for(fs::directory_iterator it(path); it != end ; ++it)
    file_names.push_back(it->path().filename().string());

  return file_names;
}

vector<string> list_file_names(string path_, vector<string> exclusions) {
  auto file_names = list_file_names(path_);
  decltype(exclusions) difference;
  sort(file_names.begin(), file_names.end());
  sort(exclusions.begin(), exclusions.end());
  set_difference(file_names.begin(), file_names.end()
               , exclusions.begin(), exclusions.end()
               , back_inserter(difference));
  return difference;
}

bool file_exists(std::string path_) {
  return fs::exists(fs::path(path_));
}
