#include <fmt/core.h>
#include <cassert>
#include <charconv>
#include <fstream>
#include <tuple>

struct line {
  int a;
  int b;
  int c;
  int d;
};

line ParseLine(const std::string &s) {
  line res;
  const char *sp = s.data();
  const char *ep = s.data() + s.size();
  sp = std::from_chars(sp, ep, res.a).ptr;
  sp = std::from_chars(sp + 1, ep, res.b).ptr;
  sp = std::from_chars(sp + 1, ep, res.c).ptr;
  sp = std::from_chars(sp + 1, ep, res.d).ptr;

  // fmt::print("{} -> ({}-{}, {}-{})\n", s, res.a, res.b, res.c, res.d);
  return res;
}

bool IsFullyContained(line l) {
  // c..a...b..d
  if ((l.a >= l.c) && (l.b <= l.d))
    return true;
  // a..c..d..b
  if ((l.c >= l.a) && (l.d <= l.b))
    return true;

  return false;
}

bool Overlaps(line l) {
  // c..a..d
  if ((l.a >= l.c) && (l.a <= l.d))
    return true;
  // c..b..d
  if ((l.b >= l.c) && (l.b <= l.d))
    return true;
  // a..c..b
  if ((l.c >= l.a) && (l.c <= l.b))
    return true;
  // a..d..b
  if ((l.d >= l.a) && (l.d <= l.b))
    return true;

  return false;
}

int main(int argc, char **argv) {
  std::string l;
  std::ifstream file("inp/day4.txt");
  uint64_t res1 = 0;
  uint64_t res2 = 0;
  while (std::getline(file, l)) {
    auto parsed = ParseLine(l);
    auto contained = IsFullyContained(parsed);
    auto overlaps = Overlaps(parsed);
    res1 += contained ? 1 : 0;
    res2 += overlaps ? 1 : 0;
    // fmt::print("{} -> {}, {}\n", l, contained, overlaps);
  }
  fmt::print("Day4: Part 1: {}\n", res1);
  fmt::print("Day4: Part 2: {}\n", res2);
}
