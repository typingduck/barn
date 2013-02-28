#include "localreport.h"

#include <boost/array.hpp>
#include <boost/asio.hpp>
#include <boost/array.hpp>
#include <sstream>

using namespace std;

void send_report(int port, const Report& report) {
  send_datagram(port, report.serialize());
}

Report receive_report(int port) {
  return Report::deserialize(receive_datagram(port));
}

const std::string Report::serialize() const {
  std::ostringstream oss;

  oss << service_name << serialization_delim
      << category << serialization_delim
      << key << serialization_delim
      << value;

  return oss.str();
}

Report Report::deserialize(const std::string& serialized) {
  std::istringstream iss(serialized);

  string service_name;
  string category;
  string key;
  int value;

  iss >> service_name
      >> category
      >> key
      >> value;

  return Report(service_name, category, key, value);
}

void send_datagram(int port, std::string message) {
  using namespace boost;
  using namespace boost::asio;
  using namespace boost::asio::ip;

  io_service io_service;
  udp::socket socket(io_service);
  socket.open(udp::v4());
  auto end_point = udp::endpoint(address(address_v4::loopback()), port);
  socket.send_to(boost::asio::buffer(message.c_str(), message.size()), end_point);
}

template<int buffer_size = 50>
string receive_datagram(int port) {
  using namespace boost;
  using namespace boost::asio;
  using namespace boost::asio::ip;

  io_service io_service;
  udp::socket socket(io_service, udp::endpoint(udp::v4(), port));
  boost::array<char, buffer_size> recv_buf;
  udp::endpoint remote_endpoint;
  auto size_read = socket.receive_from(buffer(recv_buf), remote_endpoint);
  return std::string(recv_buf.begin(), recv_buf.begin() + size_read);
}

