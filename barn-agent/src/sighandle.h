#ifndef SIGNAL_H
#define SIGNAL_H

#include <signal.h>

extern volatile sig_atomic_t child_pid_global;

void install_signal_handler();
void set_child_pid(int pid);
void unset_child_pid();
#endif


