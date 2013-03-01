#ifndef LOCALREPORT_H
#define LOCALREPORT_H

#include <string>
#include <ctime>
#include <iostream>
#include <string>
#include <boost/assign/list_of.hpp>

const std::string FilesToShip("barn_files_to_ship");
const std::string FailedToGetSyncList("barn_failed_to_get_sync_list");
const std::string RotatedDuringShip("barn_rotated_during_ship");
const std::string LostDuringShip("barn_lost_during_ship");

//These metrics will be published as zero if not occured
const std::vector<std::string> DefaultZeroMetrics =
  boost::assign::list_of(FilesToShip)
                        (FailedToGetSyncList)
                        (RotatedDuringShip)
                        (LostDuringShip);

class Report {
  public:

  const std::string service_name;
  const std::string category;
  const std::string key;
  const int value;

  Report(std::string service_name,
         std::string category,
         std::string key,
         int value)
    : service_name(service_name),
      category(category),
      key(key),
      value(value)
  {};

  const std::string serialize() const;
  static Report deserialize(const std::string& serialized);

  private:

  static const auto serialization_delim = ' ';
};

void send_report(int port, const Report& report);
Report receive_report(int port);

void send_datagram(int port, std::string message);

template<int buffer_size = 50>
std::string receive_datagram(int port);

std::pair<std::string, int> kv_pair(const Report& report);

#endif
