mkdir -p bins
#ghc -hidir bins/ -o bins/day1 -odir bins -O3 day1.hs
#ghc -hidir bins/ -o bins/day2 -odir bins -O3 day2.hs
#clang++ -g -o bins/day3 -Wall -O3 --std=c++20 day3.cpp
#clang++ -g -o bins/day3_bench -Wall -O3 --std=c++20 day3_bench.cpp
#clang++ -g -o bins/day4 -Wall -O3 --std=c++20 -lfmt day4.cpp
#clang++ -g -o bins/day5 -Wall -O3 --std=c++20 -lfmt day5.cpp
#clang++ -g -o bins/day6 -Wall -O3 --std=c++20 -lfmt day6.cpp
#clang++ -g -o bins/day7 -Wall -O3 --std=c++20 -lfmt day7.cpp
#clang++ -g -o bins/day8 -Wall -O3 --std=c++20 -lfmt -lboost_system day8.cpp
#clang++ -g -o bins/day9 -Wall -O3 --std=c++20 -lfmt -lboost_system day9.cpp
#clang++ -g -o bins/day10 -Wall -O3 --std=c++20 -lfmt day10.cpp
#clang++ -g -o bins/day11 -Wall -O3 --std=c++20 -lfmt day11.cpp
#clang++ -g -o bins/day12 -Wall -O3 --std=c++20 -lfmt day12.cpp
ghc -hidir bins/ -o bins/day13 -odir bins -O3 day13.hs

# -fsanitize=undefined -fsanitize=address 
