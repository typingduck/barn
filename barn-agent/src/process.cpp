#include "process.h"
#include <iostream>
#include <boost/process.hpp>
#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>
#include <signal.h>
#include <boost/assign/list_of.hpp>

using namespace std;
namespace bp = boost::process;
namespace ba = boost::asio;
namespace bs = boost::system;
namespace assign = boost::assign;

const std::string get_host_name() {
  string command_output = run_command("hostname", assign::list_of("hostname")("-f")).second;
  return command_output.substr(0, command_output.size()-1);
}

const pair<int, string>
run_command(const string& cmd, const vector<string>& args) {
  string exec = bp::find_executable_in_path(cmd);

  bp::posix_context ctx;
  ctx.output_behavior.insert(
    bp::behavior_map::value_type(STDOUT_FILENO, bp::capture_stream()));

  ctx.output_behavior.insert(
    bp::behavior_map::value_type(STDERR_FILENO, bp::inherit_stream()));

  bp::posix_child child = bp::posix_launch(exec, args, ctx);

  ba::io_service io_service;
  ba::signal_set signals(io_service, SIGINT, SIGTERM);


  //TODO check if boost::asio does the proper handling of handler invocation
  //otherwise I can't be doing things like this in a signal handler.
  signals.async_wait(
    [&](const bs::error_code &ec, int signal) {
      kill((pid_t)child.get_id(), signal);
      throw "I got killed, let's go shopping";
    });

  boost::thread thrd([&](){ io_service.run_one(); });

  bp::pistream &is = child.get_stdout();

  string stdout (istreambuf_iterator<char>(is), (istreambuf_iterator<char>()));

  io_service.poll();
  boost::this_thread::yield();
  io_service.stop();
  thrd.join();

  return make_pair(child.wait().exit_status(), stdout);
}
