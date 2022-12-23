#include <cassert>
#include <future>
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
#include <unordered_set>
#include <unordered_map>

struct Valve {
  std::string name;
  int flow_rate;
  std::vector<std::string> paths;
};

Valve parseValve(std::string_view line) {
  Valve valve;

  line.remove_prefix(strlen("Valve "));
  valve.name = line.substr(0, 2);
  line.remove_prefix(line.find('=') + 1);
  valve.flow_rate = std::stol(std::string{line});

  line.remove_prefix(line.find(';') + 1);
  line.remove_prefix(strlen(" tunnels lead to valve"));
  // deal with valve/valves
  line.remove_prefix(line[0] == 's' ? 2 : 1);
  
  while (!line.empty()) {
    valve.paths.push_back(std::string{line.substr(0,2)});
    line.remove_prefix(line.size() > 4 ? 4 : line.size()); // remove e.g. 'QQ, '
  }

  return valve;
}

void test_parse() {
  auto line = "Valve IK has flow rate=8; tunnels lead to valves ET, NU, ZO, XL, QD";
  auto valve = parseValve(line);
  assert(valve.name == "IK");
  assert(valve.flow_rate == 8);
  assert(valve.paths[0] == "ET");
  assert(valve.paths[1] == "NU"); 
  assert(valve.paths[2] == "ZO");
  assert(valve.paths[3] == "XL");
  assert(valve.paths[4] == "QD");
}

std::unordered_map<std::string, int> findDistances(const std::string& starting_valve, const std::unordered_set<std::string>& interesting, const std::unordered_map<std::string, Valve>& map) {
  std::deque<std::string> q;
  std::unordered_map<std::string, int> dists;
  q.push_back(starting_valve);
  dists[starting_valve] = 0;

  while (!q.empty()) {
    auto v = q.front();
    q.pop_front();
    const auto& valve = map.at(v);
    for (const auto& n : valve.paths) {
      if (!dists.contains(n)) {
        dists[n] = dists[v] + 1;
        q.push_back(n);
      }
    }
  }

  // filter dists
  return dists;
}

std::pair<int, std::vector<std::string>>
part1_go(
    const std::unordered_map<std::string, std::unordered_map<std::string, int>>& distances,
    const std::unordered_map<std::string, Valve>& map,
    const int timeLeft,
    const int totalPressure,
    const std::string& cvalve,
    std::unordered_set<std::string> unvisited) {

    if (timeLeft <= 0) return {totalPressure, {}};
    if (unvisited.size() == 0) return {totalPressure, {}};

    // fmt::print("{:\t>{}}", "", (30 - timeLeft) / 2);
    // fmt::print("opened {} (tl:{}, pressure:{}))\n", cvalve, timeLeft, totalPressure);
    int best = totalPressure;
    std::vector<std::string> bestV;

    for (const std::string& unv : unvisited) {
      auto dist = distances.at(cvalve).at(unv);
      if (dist >= timeLeft + 1) continue;
      auto& valve = map.at(unv);
      auto pathTime = timeLeft - dist - 1;
      auto pathPressure = totalPressure + (pathTime * valve.flow_rate);
      auto unvp = unvisited;
      unvp.erase(unv);
      auto [r,v] = part1_go(distances, map, pathTime, pathPressure, unv, unvp);
      if (r > best) { 
        // fmt::print("{: >{}}", "", (30 - timeLeft) / 2);
        // fmt::print("opening {} next was better ({} -> {})\n", unv, best, r);
        best = r; 
        bestV = v;
        bestV.push_back(unv);
      }
    }
    return {best, bestV};
}

int part1(const std::unordered_map<std::string, std::unordered_map<std::string, int>>& distances,
    const std::unordered_set<std::string>& interesting,
    const std::unordered_map<std::string, Valve>& map) {

  return part1_go(distances, map, 30, 0, "AA", interesting).first;
}

int part2(const std::unordered_map<std::string, std::unordered_map<std::string, int>>& distances,
    const std::unordered_set<std::string> interesting,
    const std::unordered_map<std::string, Valve>& map) {

  constexpr int kThreadCount = 8;

  auto threadF = [&](int offset) {
    int best = 0;
    uint64_t count = (1 << interesting.size());
    for (int i = offset; i < count; i+=kThreadCount) {
      std::unordered_set<std::string> interesting1;
      std::unordered_set<std::string> interesting2;
      int x = 0;
      for (const auto& inte : interesting) {
        if ((i & (1 << x++)) == 0) {
          interesting1.insert(inte);
        } else {
          interesting2.insert(inte);
        }
      }
      // fmt::print("i:{} x:{} [{}] / [{}]\n", i, x, fmt::join(interesting1, ","), fmt::join(interesting2, ","));
      auto me = part1_go(distances, map, 26, 0, "AA", interesting1);
      auto ele = part1_go(distances, map, 26, 0, "AA", interesting2);
      if (me.first + ele.first > best) {
        best = me.first + ele.first;
        fmt::print("New best {} ({} + {}): [{}] / [{}]\n", best, me.first, ele.first, fmt::join(me.second, ","), fmt::join(ele.second, ","));
      }
    }
    return best;
  };

  std::vector<std::future<int>> threadResults;
  for (int i = 0; i < kThreadCount; i++) {
    threadResults.emplace_back(std::async(threadF, i));
  }
  int res = 0;
  for (int i = 0; i < kThreadCount; i++) {
    auto x = threadResults[i].get();
    if (x > res) {
      res = x;
    }
  }
  return res;
}

int main(int argc, char **argv) {
  // test_parse();

  // std::ifstream file("inp/day16-test.txt");
  std::ifstream file("inp/day16.txt");
  std::string l;
  std::unordered_map<std::string, Valve> valves;
  std::unordered_set<std::string> interestingValves = {"AA"};
  while (std::getline(file, l)) {
    auto v = parseValve(l);
    if (v.flow_rate > 0) interestingValves.insert(v.name);
    valves[v.name] = v;
  }

  // fmt::print("Interesting valves: {}\n", fmt::join(interestingValves, ", "));
  // find the distance from every interesting room to every other
  std::unordered_map<std::string, std::unordered_map<std::string, int>> distances;
  for (const auto& room : interestingValves) {
    distances[room] = findDistances(room, interestingValves, valves);
  }

  // fmt::print("dists from valve AA: {}\n", fmt::join(distances["AA"], ", "));

  // now just kind of want to try everything I guess?
  interestingValves.erase("AA");
  auto part1res = part1(distances, interestingValves, valves);
  fmt::print("Day16: Part1: {}\n", part1res);

  auto part2res = part2(distances, interestingValves, valves);
  fmt::print("Day16: Part2: {}\n", part2res);
}
