#include "helpers.h"

#include <vector>
#include <string>
#include <sstream>

using namespace std;

const vector<string> split(string str, char delim) {
  std::stringstream stream(str);
  vector<string> elements;
  while(!stream.eof()) {
    std::string line;
    getline(stream, line, delim);

    if(!stream.eof() || line != "")
      elements.push_back(line);
  }
  return elements;
}


const vector<string> prepend_each(vector<string> vec, string prefix) {
  vector<string> new_vec;

  for(vector<string>::const_iterator it = vec.begin(); it < vec.end(); ++it) {
    new_vec.push_back(prefix + *it);
  }

  return new_vec;

}

bool contained(vector<string> small, vector<string> big) {
  bool valid = true;
  for(vector<string>::iterator it = small.begin(); it < small.end(); ++it)
    valid &= std::find(big.begin(), big.end(), *it) != big.end();
  return valid;
}

