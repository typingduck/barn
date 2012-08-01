#cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 500
size_in_mb=5
sizein_bytes=$(($size_in_mb * 1024 * 1024))

while true; do
  dd if=/dev/urandom bs=$sizein_bytes count=1 | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 500
  sleep 1
done
