#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <boost/filesystem/operations.hpp>

typedef boost::filesystem::filesystem_error fs_error;

std::vector<std::string> list_file_paths(std::string path);
std::vector<std::string> list_file_names(std::string path);
std::vector<std::string> list_file_names(std::string path
                                       , std::vector<std::string> exclusions);
bool file_exists(std::string path);

#endif

