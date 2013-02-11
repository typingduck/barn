#include "helpers.h"

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <boost/algorithm/string.hpp>

using namespace std;

const vector<string> split(string str, char delim) {
  vector<string> tokens;
  boost::split(tokens, str, boost::is_any_of(string(1, delim)));
  return tokens;
}

const vector<string> prepend_each(vector<string> vec, string prefix) {
  vector<string> new_vec;

  for(string& el : vec)
    new_vec.push_back(prefix + el);

  return new_vec;

}

bool contained(vector<string> small, vector<string> big) {
  bool valid = true;

  //TODO not efficient, make me better.
  for(string& el : small)
    valid &= std::find(big.begin(), big.end(), el) != big.end();

  return valid;
}

