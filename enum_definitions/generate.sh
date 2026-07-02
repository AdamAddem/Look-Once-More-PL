echo "What enum to generate?"
read name

g++ -std=c++23 enum_generator.cpp -O0 -o a.out
./a.out "$name"
rm a.out
