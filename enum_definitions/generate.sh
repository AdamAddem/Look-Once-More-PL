echo "What enum to generate?"
read name

g++ -std=c++23 tokentype_generator.cpp -o a.out
./a.out "$name"
rm a.out
