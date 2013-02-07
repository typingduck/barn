#include <iostream>
#include <vector>
#include "files.h"
#include "process.h"
#include "helpers.h"
#include <unistd.h>

using namespace std;

//TODO make me better. ls isn't nice but alternatives without boost are complicated atm.
vector<string> list_files(string path) {
  const pair<bool, string> ls_result = run_command("ls " + path + " | cat");
  return split(ls_result.second, '\n');
}

bool file_exists(std::string path) {
  return access( path.c_str(), F_OK ) != -1;
}
