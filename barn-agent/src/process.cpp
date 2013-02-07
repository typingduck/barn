#include "process.h"

using namespace std;

const std::string get_host_name() {
  string command_output = run_command("hostname -f").second;
  return command_output.substr(0, command_output.size()-1);
}

//Run a command, grabs its stdout and exit status
//This is not particularly the best way to do this
//Since it relies on the system shell interpreter
const pair<int, string> run_command(const string& cmd) {
  const int MAX_BUFFER = 255;
  char buffer[MAX_BUFFER];
  string stdout;
  FILE* stream = popen(("exec " + cmd).c_str(), "r");

  while(fgets(buffer, MAX_BUFFER, stream) != NULL)
    stdout.append(buffer);

  int exit_code = pclose(stream);
  return make_pair(exit_code, stdout);
}

