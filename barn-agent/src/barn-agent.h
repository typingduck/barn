#ifndef BARN_AGENT_H
#define BARN_AGENT_H

#include <vector>
#include <string>
#include <boost/assign/list_of.hpp>
#include <boost/variant.hpp>

#include "helpers.h"

const auto space = " ";
const auto token_separator = "@";
const auto path_separator = "/";

const auto remote_rsync_namespace = "barn_logs";

const auto rsync_exclusions = prepend_each(
  boost::assign::list_of<std::string>("*.u")("config")("current")("lock")("*~"), "--exclude=");

const auto rsync_flags = boost::assign::list_of<std::string>("--times")("--verbose");

typedef std::string FileName;
typedef std::vector<std::string> FileNameList;

typedef std::string BarnError;

template <typename T>
using Validation = typename boost::variant<BarnError, T>;

template<typename T, typename SuccessFunc, typename FailureFunc>
void fold(Validation<T> v,
          SuccessFunc success,
          FailureFunc failure) {
  T* success_value = boost::get<T*>(v);
  BarnError* error_value = boost::get<BarnError*>(v);

  if(success_value != 0)
    success(*success_value);
  else
    failure(*error_value);
}

struct BarnConf {
  std::string barn_rsync_addr;
  std::string rsync_source;
  std::string service_name;
  std::string category;
  bool monitor_mode;
  int monitor_port;
};

struct ShipStatistics {
  ShipStatistics(int num_shipped, int num_rotated_during_ship)
    :num_shipped(num_shipped),
     num_rotated_during_ship(num_rotated_during_ship) {};

  int num_shipped;
  int num_rotated_during_ship;
};

bool sync_files(const BarnConf& barn_conf);

bool sleep_it(const BarnConf& barn_conf);

const BarnConf parse_command_line(int argc, char* argv[]);

Validation<FileNameList> query_candidates(const BarnConf& barn_conf);

Validation<ShipStatistics>
ship_candidates(std::vector<std::string> candidates,
                     const BarnConf& barn_conf);

#endif

