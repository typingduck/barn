#include "sighandle.h"

#include <stdio.h>
#include <unistd.h>

static const int INVALID_PID = -1;

//Declared in sighandle.h as extern, here is the once-only definition of
// the extern variable.
volatile sig_atomic_t child_pid_global = INVALID_PID;

struct sigaction sa, old;

static void signal_handler (int ignore)
{
  if(child_pid_global != INVALID_PID)
    kill (child_pid_global, SIGTERM);

  // Unset the signal handler to the default
  // so the next kill signal will do the default kill.
  sigaction (SIGTERM, &old, 0);
  kill (getpid(), SIGTERM);
}

void enable_kill_child_signal_handler() {
  sa.sa_handler = signal_handler;
  sigemptyset (&sa.sa_mask);
  sa.sa_flags = 0;
  sigaction (SIGTERM, &sa, &old);
}

void set_child_pid(int pid) {
  child_pid_global = pid;
}

void unset_child_pid() {
  child_pid_global = INVALID_PID;
}
