#include <string>

#ifndef PROCESS_H
#define PROCESS_H

const std::pair<int, std::string> run_command(const std::string& cmd);
const std::string get_host_name();
#endif
