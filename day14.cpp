#include <cassert>
#include <deque>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <functional>
#include <charconv>
#include <string>
#include <string_view>
#include <vector>

using MapT = std::vector<std::vector<bool>>;
static constexpr int kWidth = 1024;

std::pair<int, int> parseCoord(std::string_view& sv) {
  int x, y;
  auto res = std::from_chars(sv.data(), sv.data() + sv.size(), x);
  assert(res.ec == std::errc());
  sv.remove_prefix(res.ptr - sv.data());
  assert(sv[0] == ',');
  sv.remove_prefix(1);
  res = std::from_chars(sv.data(), sv.data() + sv.size(), y);
  assert(res.ec == std::errc());
  sv.remove_prefix(res.ptr - sv.data());
  assert(sv.size() == 0 || sv[0] == ' ');
  // flip the coordinates because I've already written
  // it backwards...
  return {y,x};
}

void extendMap(MapT& m, int size) {
  int curSz = m.size();
  if (curSz >= size + 1) return;
  m.resize(size + 1);
  for (int i = curSz; i < m.size(); i++) {
    m[i].resize(kWidth, false);
  }
}

void drawLine(MapT& map, std::pair<int, int> coord1, std::pair<int, int> coord2) {
  if (coord1.first == coord2.first) {
    // Same row
    assert(coord1.second < kWidth && coord1.first >= 0);
    assert(coord2.second < kWidth && coord2.first >= 0);

    const auto row = coord1.first;
    extendMap(map, row);
    for (int i = std::min(coord1.second, coord2.second); i <= std::max(coord1.second, coord2.second); i++) {
      map[row][i] = true;
    }
  } else {
    // same column
    assert(coord1.second == coord2.second);
    extendMap(map, coord1.first);
    extendMap(map, coord2.first);
    const auto col = coord1.second;

    for (int i = std::min(coord1.first, coord2.first); i <= std::max(coord1.first, coord2.first); i++) {
      map[i][col] = true;
    }
  }
}

void fillMap(MapT& map, std::string_view line) {
  auto coord = parseCoord(line);
  while (!line.empty()) {
    assert(line.starts_with(" -> "));
    line.remove_prefix(4);
    auto coord2 = parseCoord(line);
    drawLine(map, coord, coord2);
    coord = coord2;
  }
}

int dropSand(MapT& map, int starts) {
  int res = 0;

  while (true) {
    std::pair<int, int> loc = {0, starts};
    res++;
    while (true) {
      if (loc.first == map.size() - 1) return res - 1;
      if (!map[loc.first + 1][loc.second]) {
        loc.first++;
      } else if (!map[loc.first + 1][loc.second - 1]) {
        loc.first++;
        loc.second--;
      } else if (!map[loc.first + 1][loc.second + 1]) {
        loc.first++;
        loc.second++;
      } else {
        map[loc.first][loc.second] = true;
        break;
      }
    }
  }
}

int dropSand2(MapT& map, int starts) {
  int res = 0;
  std::fill(map.back().begin(), map.back().end(), true);

  while (!map[0][starts]) {
    std::pair<int, int> loc = {0, starts};
    res++;
    while (true) {
      if (!map[loc.first + 1][loc.second]) {
        loc.first++;
      } else if (!map[loc.first + 1][loc.second - 1]) {
        loc.first++;
        loc.second--;
      } else if (!map[loc.first + 1][loc.second + 1]) {
        loc.first++;
        loc.second++;
      } else {
        map[loc.first][loc.second] = true;
        break;
      }
    }
  }
  return res;
}

int main(int argc, char **argv) {
  // std::ifstream file("inp/day14-test.txt");
  std::ifstream file("inp/day14.txt");
  std::string l;

  MapT map;

  while (std::getline(file, l)) {
    fillMap(map, l);
  }
  extendMap(map, map.size() + 1);
  MapT map2 = map; // Save for part 2
  int part1 = dropSand(map, 500);
  fmt::print("Day14: Part1: {}\n", part1);

  int part2 = dropSand2(map2, 500);
  fmt::print("Day14: Part2: {}\n", part2);
}
