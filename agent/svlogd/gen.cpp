#include <iostream>
#include <cstdlib>

using namespace std;

//Code copied from http://stackoverflow.com/questions/440133/how-do-i-create-a-random-alpha-numeric-string-in-c
void gen_random(char *s, const int len) {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < len; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    s[len] = 0;
}

int main(int argc, char* argv[]) {

  if(argc < 2) {
    cout << "Usage: randomshit MB_OF_SHIT_TO_PRODUCE" << endl;
    exit(0);
  }
  const int max_len = 500;
  char* buffer = new char[max_len];
  int len = 0;
  int mb = atoi(argv[1]);

  cerr << "Writing " << mb << " MB of random lines to stdout" << endl;

  int max_bytes = mb * 1024 * 1024;
  int made_bytes = 0;

  while(made_bytes <= max_bytes) {
    len = rand() % (max_len - 1);
    gen_random(buffer, len);
    made_bytes += len + 1;
    cout << buffer << "\n";
  }

}
