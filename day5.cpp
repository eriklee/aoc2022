#include <cassert>
#include <charconv>
#include <deque>
#include <fmt/core.h>
#include <fstream>
#include <string_view>
#include <vector>

using Stacks = std::vector<std::deque<char>>;

void printStacks(const Stacks &stacks) {
  auto stackCount = stacks.size();
  for (int i = 0; i < stackCount; i++) {
    fmt::print("{}: ", i + 1);
    for (const auto c : stacks[i])
      fmt::print("{} ", c);
    fmt::print("\n");
  }
}

Stacks parseStacks(std::ifstream &file) {
  Stacks stacks;

  std::string l;
  std::getline(file, l);
  int stackCount = (l.size() + 1) / 4;
  stacks.resize(stackCount);

  while (!l.empty()) {
    for (int i = 0; i < stackCount; i++) {
      int idx = i * 4 + 1;
      char c = l.at(idx);
      if (c >= 'A' && c <= 'Z') {
        stacks[i].push_front(c);
      }
    }
    std::getline(file, l);
  }

  // printStacks(stacks);

  return stacks;
}

struct Inst {
  int cnt;
  int source;
  int target;
};

int parseInt(std::string_view &s) {
  int res;
  auto fcr = std::from_chars(s.data(), s.data() + s.size(), res);
  assert(fcr.ec == std::errc());
  s.remove_prefix(fcr.ptr - s.data());

  return res;
}

Inst parseInst(std::string_view s) {
  Inst res;

  auto pos = s.find(' ');
  assert(pos != std::string_view::npos);
  s.remove_prefix(pos + 1);
  res.cnt = parseInt(s);

  pos = s.find(' ', 1);
  assert(pos != std::string_view::npos);
  s.remove_prefix(pos + 1);

  res.source = parseInt(s);

  pos = s.find(' ', 1);
  assert(pos != std::string_view::npos);
  s.remove_prefix(pos + 1);

  res.target = parseInt(s);
  // fmt::print("move {} from {} to {}\n", res.cnt, res.source, res.target);
  return res;
}

void perform(const Inst inst, Stacks &stacks) {
  for (int i = 0; i < inst.cnt; i++) {
    char c = stacks[inst.source - 1].back();
    stacks[inst.source - 1].pop_back();
    stacks[inst.target - 1].push_back(c);
  }
}

void performPart2(const Inst inst, Stacks &stacks) {
  auto &source = stacks[inst.source - 1];
  auto &target = stacks[inst.target - 1];

  auto sourceSz = source.size();

  for (int i = sourceSz - inst.cnt; i < sourceSz; i++) {
    char c = source.at(i);
    target.push_back(c);
  }
  source.resize(sourceSz - inst.cnt);
}

int main(int argc, char **argv) {
  std::ifstream file("inp/day5.txt");

  auto stacks = parseStacks(file);
  auto stacks2 = stacks;

  std::string l;
  while (std::getline(file, l)) {
    auto inst = parseInst(l);
    perform(inst, stacks);
    performPart2(inst, stacks2);
    // printStacks(stacks2);
  }

  l.clear();
  for (const auto &s : stacks) {
    l.push_back(s.back());
  }
  fmt::print("Day5: Part 1: {}\n", l);

  l.clear();
  for (const auto &s : stacks2) {
    l.push_back(s.back());
  }
  fmt::print("Day5: Part 2: {}\n", l);
}
