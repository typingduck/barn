#ifndef LOCALREPORT_H
#define LOCALREPORT_H

#include <string>
#include <ctime>
#include <iostream>
#include <string>

const std::string FilesToShip("files_to_ship");
const std::string FailedToGetSyncList("failed_to_get_sync_list");

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
