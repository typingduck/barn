g++ -o loggen gen.cpp

size_in_mb=$1

while true; do
  ./loggen $size_in_mb
  sleep 1
done
