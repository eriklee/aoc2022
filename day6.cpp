#include <bitset>
#include <fmt/core.h>
#include <fstream>

int day6(const int distinct, const std::string &inp) {
  std::bitset<26> bits;

  int i = distinct - 1;
  for (; i < inp.size(); i++) {
    bits.reset();
    for (int j = i - (distinct - 1); j <= i; j++) {
      bits.set(inp[j] - 'a');
    }
    if (bits.count() == distinct)
      break;
  }
  return i + 1;
}

int main(int argc, char **argv) {
  std::ifstream file("inp/day6.txt");
  std::string l;
  std::getline(file, l);

  auto p1 = day6(4, l);
  fmt::print("Day6: Part 1: {}\n", p1);
  auto p2 = day6(14, l);
  fmt::print("Day6: Part 2: {}\n", p2);
}
