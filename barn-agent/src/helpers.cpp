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


//Returns the number of elements from small missing in big
int count_missing(vector<string> small, vector<string> big) {
  int invalid = 0;

  //TODO not efficient, make me better.
  for(string& el : small)
    invalid += std::find(big.begin(), big.end(), el) == big.end();

  return invalid;
}

