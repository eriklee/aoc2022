set -x
mkdir -p bins

DBG_OPTS="-fsanitize=undefined -fsanitize=address -Wall --std=c++20 -lfmt -g -O0"
OPTS="-Wall --std=c++20 -lfmt -g -O3"

#ghc -hidir bins/ -o bins/day1 -odir bins -O3 day1.hs
#ghc -hidir bins/ -o bins/day2 -odir bins -O3 day2.hs
#clang++ $OPTS -o bins/day3 day3.cpp
#clang++ $OPTS -o bins/day3_bench day3_bench.cpp
#clang++ $OPTS -o bins/day4 day4.cpp
#clang++ $OPTS -o bins/day5 day5.cpp
#clang++ $OPTS -o bins/day6 day6.cpp
#clang++ $OPTS -o bins/day7 day7.cpp
#clang++ $OPTS -o bins/day8 -lboost_system day8.cpp
#clang++ $OPTS -o bins/day9 -lboost_system day9.cpp
#clang++ $OPTS -o bins/day10 day10.cpp
#clang++ $OPTS -o bins/day11 day11.cpp
#clang++ $OPTS -o bins/day12 day12.cpp
#ghc -hidir bins/ -o bins/day13 -odir bins -O3 day13.hs
clang++ $OPTS -o bins/day14 day14.cpp
