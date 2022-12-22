#include <cassert>
#include <charconv>
#include <deque>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <functional>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

struct LineInfo {
  int64_t sat_x;
  int64_t sat_y;
  int64_t beacon_x;
  int64_t beacon_y;
};

LineInfo parseLine(std::string_view sv) {
  LineInfo res;
  sv.remove_prefix(sv.find('=') + 1);
  auto fcres = std::from_chars(sv.data(), sv.data() + sv.size(), res.sat_x);
  assert(fcres.ec == std::errc());

  sv.remove_prefix(sv.find('=') + 1);
  fcres = std::from_chars(sv.data(), sv.data() + sv.size(), res.sat_y);
  assert(fcres.ec == std::errc());

  sv.remove_prefix(sv.find('=') + 1);
  fcres = std::from_chars(sv.data(), sv.data() + sv.size(), res.beacon_x);
  assert(fcres.ec == std::errc());

  sv.remove_prefix(sv.find('=') + 1);
  fcres = std::from_chars(sv.data(), sv.data() + sv.size(), res.beacon_y);
  assert(fcres.ec == std::errc());

  return res;
}

void test_parse() {
  auto l = "Sensor at x=2881987, y=1923522: closest beacon is at x=2528182, "
           "y=2000000";
  auto li = parseLine(l);
  assert(li.sat_x == 2881987);
  assert(li.sat_y == 1923522);
  assert(li.beacon_x == 2528182);
  assert(li.beacon_y == 2000000);
}

std::optional<std::pair<int64_t, int64_t>> getCover(const int64_t lineIndex,
                                            const LineInfo &li) {
  auto dist =
      std::abs(li.sat_x - li.beacon_x) + std::abs(li.sat_y - li.beacon_y);

  int64_t dist_to_line = std::abs(li.sat_y - lineIndex);
  if (dist_to_line > dist)
    return std::nullopt;

  auto rem = dist - dist_to_line;
  return {{li.sat_x - rem, li.sat_x + rem}};
}

void test_getCover() {
  auto li = parseLine("Sensor at x=8, y=7: closest beacon is at x=2, y=10");
  auto cover = getCover(10, li);
  assert(cover);
  assert(cover->first == 2);
  assert(cover->second == 14);
}

bool tryMerge(std::pair<int64_t, int64_t> &a, const std::pair<int64_t, int64_t> &b) {
  assert(a.first <= b.first); // they don't overlap
  if (b.first > a.second + 1)
    return false;
  a.second = std::max(a.second, b.second); // (ax... bx.. [ay...by])
  return true;
}

int main(int argc, char **argv) {
  {
    // std::ifstream file("inp/day15-test.txt");
    std::ifstream file("inp/day15.txt");
    std::string l;

    test_parse();
    test_getCover();

    std::vector<std::pair<int64_t, int64_t>> line_overlaps;

    static constexpr int64_t lineIndex = 2000000;
    // static constexpr int64_t lineIndex = 10;

    while (std::getline(file, l)) {
      auto ov = getCover(lineIndex, parseLine(l));
      // fmt::print("{}: \n", l);
      if (ov) {
        line_overlaps.push_back(*ov);
        fmt::print("({} -> {})\n", ov->first, ov->second);
      }
    }
    std::sort(line_overlaps.begin(), line_overlaps.end(),
              [](const auto a, const auto b) { return a.first < b.first; });
    auto it = line_overlaps.begin();
    while (it != line_overlaps.end()) {
      while (it + 1 != line_overlaps.end() && tryMerge(*it, *(it + 1))) {
        fmt::print("merging {} and {}\n", *it, *(it + 1));
        line_overlaps.erase(it + 1);
      }
      it++;
    }
    // we should be left with only non-overlapping areas now, so just add up the
    // sizes
    int res1 = 0;
    for (const auto &[x, y] : line_overlaps) {
      res1 += (y - x);
    }
    fmt::print("Day15: Part1: {}\n", res1);
  }

  // std::ifstream file("inp/day15-test.txt");
  std::ifstream file("inp/day15.txt");
  std::string l;

  std::vector<LineInfo> lis;

  // static constexpr int maxSz = 20;
  static constexpr int64_t maxSz = 4000000;
  while (std::getline(file, l)) {
    lis.push_back(parseLine(l));
  }

  std::vector<std::pair<int64_t, int64_t>> line_overlaps;

  for (int64_t y = 0; y < maxSz; y++) {
    line_overlaps.clear();
    for (const auto &li : lis) {
      auto ov = getCover(y, li);
      if (ov)
        line_overlaps.push_back(*ov);
    }
    std::sort(line_overlaps.begin(), line_overlaps.end(),
              [](const auto a, const auto b) { return a.first < b.first; });
    auto it = line_overlaps.begin();
    while (it != line_overlaps.end()) {
      while (it + 1 != line_overlaps.end() && tryMerge(*it, *(it + 1))) {
        line_overlaps.erase(it + 1);
      }
      it++;
    }
    if (line_overlaps.size() >= 2) {
      // fmt::print("non-0 overlaps: {}: {}\n", x, fmt::join(line_overlaps, ","));
      fmt::print("Day15: Part2: {}\n", maxSz * (line_overlaps[0].second + 1) + y);
    }
  }
}
