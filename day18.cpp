#include <cassert>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <optional>
#include <string>
#include <vector>
#include <charconv>
#include <array>
#include <boost/container_hash/hash.hpp>
#include <boost/container/flat_set.hpp>

struct Cube {
  int x;
  int y;
  int z;
  auto operator<=>(const Cube&) const = default;
};

using Map = boost::container::flat_set<Cube>;

Cube parseLine(std::string_view sv) {
  auto ptr = sv.data();
  const auto end = sv.data() + sv.size();
  Cube c;
  auto r = std::from_chars(ptr, end, c.x);
  assert(r.ec == std::error_code());
  assert(*r.ptr == ',');
  ptr = r.ptr + 1;

  r = std::from_chars(ptr, end, c.y);
  assert(r.ec == std::error_code());
  assert(*r.ptr == ',');
  ptr = r.ptr + 1;

  r = std::from_chars(ptr, end, c.z);
  assert(r.ec == std::error_code());
  assert(r.ptr == end);

  return c;
}

void test_parse() {
  auto c = parseLine("16,8,19");
  assert(c.x == 16); assert(c.y == 8); assert(c.z == 19);

  c = parseLine("0,8,8");
  assert(c.x == 0); assert(c.y == 8); assert(c.z == 8);
}

int countAdjacent(Map& map, Cube c) {
  int res = 0;

  c.x -= 1;
  if (map.contains(c)) res++;
  c.x += 2;
  if (map.contains(c)) res++;
  c.x -= 1;
  c.y -= 1;
  if (map.contains(c)) res++;
  c.y += 2;
  if (map.contains(c)) res++;
  c.y -= 1;
  c.z -= 1;
  if (map.contains(c)) res++;
  c.z += 2;
  if (map.contains(c)) res++;

  return res;
}

void maxCube(Cube& t, const Cube& n) {
  t.x = std::max(t.x, n.x);
  t.y = std::max(t.y, n.y);
  t.z = std::max(t.z, n.z);
}

void minCube(Cube& t, const Cube& n) {
  t.x = std::min(t.x, n.x);
  t.y = std::min(t.y, n.y);
  t.z = std::min(t.z, n.z);
}

int main(int argc, char** argv) {
  // test_parse();
  // std::ifstream file("inp/day18-test.txt");
  std::ifstream file("inp/day18.txt");
  std::string l;
  Map map;

  int part1 = 0;
  Cube mx, mn;
  mx.x = mx.y = mx.z = 0;
  mn.x = mn.y = mn.z = 1000000;

  while (std::getline(file, l)) {
    auto cube = parseLine(l);
    auto neighbors = countAdjacent(map, cube);
    minCube(mn, cube);
    maxCube(mx, cube);
    map.insert(cube);
    part1 += 6 - (2 * neighbors);
  }
  fmt::print("Day18: Part1: {}\n", part1);

  Map invMap;
  mn.x -= 1; mn.y -= 1; mn.z -= 1;
  mx.x += 1; mx.y += 1; mx.z += 1;
  std::deque<Cube> q {mn};

  while (!q.empty()) {
    Cube c = q.front();
    q.pop_front();
    if (map.contains(c)) continue;
    if (invMap.contains(c)) continue;

    invMap.insert(c);
    if (c.x >= mn.x) q.push_back({c.x - 1, c.y, c.z});
    if (c.x <= mx.x) q.push_back({c.x + 1, c.y, c.z});
    if (c.y >= mn.y) q.push_back({c.x, c.y - 1, c.z});
    if (c.y <= mx.y) q.push_back({c.x, c.y + 1, c.z});
    if (c.z >= mn.z) q.push_back({c.x, c.y, c.z - 1});
    if (c.z <= mx.z) q.push_back({c.x, c.y, c.z + 1});
  }
  int part2 = 0;
  for (int x = mn.x; x <= mx.x; x++) {
    for (int y = mn.y; y <= mx.y; y++) {
      for (int z = mn.z; z <= mx.z; z++) {
        Cube c = {x,y,z};
        if (!invMap.contains(c)) part2 += countAdjacent(invMap, c);
      }
    }
  }

  fmt::print("Day18: Part2: {}\n", part2);
}
