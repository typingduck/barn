while inotifywait -t 1 --exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~' --exclude '\.(swx|swp)' -q -e close_write logs || true; do
  rsync --exclude '*u' --size-only --verbose logs/* rsync://$1:$2/barn_logs
done
