#ifndef HELPERS_H
#define HELPERS_H

#include <vector>
#include <string>

const std::vector<std::string> split(std::string str, char delim);
const std::vector<std::string> prepend_each(std::vector<std::string> vec, std::string prefix);
bool contained(vector<string> small, vector<string> big);

#endif
