mkdir -p bins
ghc -O day1 && mv day1 bins/
ghc -O day2 && mv day2 bins/
clang++ -g -o bins/day3 -Wall -O3 --std=c++20 day3.cpp
clang++ -g -o bins/day3_bench -Wall -O3 --std=c++20 day3_bench.cpp
clang++ -g -o bins/day4 -Wall -O3 --std=c++20 -lfmt day4.cpp
clang++ -g -o bins/day5 -Wall -O3 --std=c++20 -lfmt day5.cpp
clang++ -g -o bins/day6 -Wall -O3 --std=c++20 -lfmt day6.cpp
