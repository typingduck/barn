

/*
 * Time based channel selector for choosing between primary and
 * secondary (backup) endpoints.
 * A heartbeat is used to keep a channel alive and by calling it within
 * 'seconds_before_failover' time.
 *
 * The channel picker will alternate between broken primary and secondary
 * every 'seconds_before_failover'. So if a primary channel recovers it
 * wil be reused again.
 *
 * Example Usage:
 *     cs = C
 *     while (true)
 *         channel = cs.pick_channel()
 *         // or
 *         if (primary_channel_ok)
 *             cs.heartbeat()
 */
template <class T> class ChannelSelector {

public:
    ChannelSelector(T primary, T secondary, int seconds_before_failover):
        primary(primary),
        secondary(secondary),
        seconds_before_failover(seconds_before_failover) {
      assert(seconds_before_failover > 0);
      primary_ok = true;
      last_heartbeat_time = now_in_seconds();
   };

  const T current() const {
    if (primary_ok)
        return primary;
    else
        return secondary;
  }

  void heartbeat() {
    if(primary_ok) {
        last_heartbeat_time = now_in_seconds();
    }
  }

  T pick_channel() {
    time_t now = now_in_seconds(); 
    time_t time_since_heartbeat = now - last_heartbeat_time;
    if (primary_ok &&
        time_since_heartbeat < seconds_before_failover) {
      // normal case, everything ok
    } else if (primary_ok) {
      // too long, perform failover
      std::cout << "!!Channel: error primary down for too long, failing to backup" << std::endl;
      primary_ok = false;
      last_heartbeat_time = now;
    } else if (time_since_heartbeat < seconds_before_failover) {
      // on secondary, stay there for now
    } else {
      // on secondary for long enough, try primary again
      std::cout << "!!Channel: trying to fail back to primary from backup" << std::endl;
      primary_ok = true;
      last_heartbeat_time = now;
    }
    return current();
  }

protected:
  // protected for testing.
  virtual time_t now_in_seconds() const {
    time_t t = time(0);
    if (t < 0) {
        throw "Failed to get time from system";
    }
    return t;
  }

  // On primary channel: time since last heartbeat() function call
  // On secondary channel: time since flip from primary to secondary
  time_t last_heartbeat_time;

private:
    T primary, secondary;
    bool primary_ok;
    time_t seconds_before_failover;
};
