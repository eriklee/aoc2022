#include <cassert>
#include <cstdint>
#include <fstream>
#include <string>
#include <string_view>

uint64_t priority(char c) {
  return 1ul << ((c < 'a') ? (c - 'A' + 27) : (c - 'a' + 1));
}

int priorityToNum(uint64_t p) {
  uint64_t res = 0;
  while (p != 0) {
    int n = __builtin_ctzl(p);
    res += n;
    p ^= (1ul << n);
  }
  return res;
}

uint64_t catalogueItems(std::string_view s) {
  uint64_t has = 0;
  for (const auto c : s) {
    auto prio = priority(c);
    has |= prio;
  }
  return has;
}

uint64_t findSharedPriority(const std::string &s) {
  auto compartment_size = s.size() / 2;
  std::string_view sv(s);
  auto comp_1 = catalogueItems(sv.substr(0, compartment_size));
  auto comp_2 = catalogueItems(sv.substr(compartment_size));
  return comp_1 & comp_2;
}

int main(int argc, char **argv) {
  assert(priorityToNum(priority('a')) == 1);
  assert(priorityToNum(priority('z')) == 26);
  assert(priorityToNum(priority('A')) == 27);
  assert(priorityToNum(priority('P')) == 42);
  assert(priorityToNum(priority('Z')) == 52);

  std::string l;
  {
    std::ifstream file("inp/day3.txt");
    uint64_t sum = 0;
    while (std::getline(file, l)) {
      auto priority = findSharedPriority(l);
      sum += priorityToNum(priority);
      // printf("%s -> %d\n", l.c_str(), priorityToNum(priority));
    }
    printf("Day3: Part 1: %lu\n", sum);
  }
  {
    std::ifstream file("inp/day3.txt");
    uint64_t sum = 0;
    while (std::getline(file, l)) {
      uint64_t common = catalogueItems(l);
      assert(std::getline(file, l));
      common &= catalogueItems(l);
      assert(std::getline(file, l));
      common &= catalogueItems(l);
      // printf("%s -> %d\n", l.c_str(), priorityToNum(common));
      assert(__builtin_popcountl(common) == 1);

      sum += priorityToNum(common);
    }
    printf("Day3: Part 2: %lu\n", sum);
  }
}
