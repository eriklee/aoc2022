#include <cassert>
#include <deque>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <functional>
#include <string>
#include <vector>

int bfs(std::deque<std::pair<int, int>>& queue, const std::vector<std::string>& map, std::pair<int, int> goalPos) {
  std::vector<std::vector<int>> path_lens;
  path_lens.resize(map.size());
  for (auto& pls : path_lens) { pls.resize(map[0].size(), -1); }

  for (const auto& [r,c] : queue) {
    path_lens[r][c] = 0;
  }

  auto valid_move = [&](int r, int c, int cur_r, int cur_c) {
    return (path_lens[r][c] == -1) && ((map[r][c] - 1) <= map[cur_r][cur_c]);
  };

  // assert(valid_move(1, 0, 0, 0));
  // assert(valid_move(0, 1, 0, 0));
  // assert(valid_move(1, 1, 1, 0));
  // assert(!valid_move(2, 1, 2, 0));

  while (!queue.empty() &&
         (path_lens[goalPos.first][goalPos.second] == -1)) {
    auto [r, c] = queue.front();
    queue.pop_front();

    const auto cpath = path_lens[r][c];
    // fmt::print("Checking out ({},{}) with path_len {}, val={}\n", r, c, cpath.length, map[r][c]);
    if (r > 0) {
      if (valid_move(r - 1, c, r, c)) {
        queue.push_back({r - 1, c});
        path_lens[r - 1][c] = cpath + 1;
        // fmt::print("Added ({},{}) with path_len {}\n", r - 1, c, cpath.length + 1);
      }
    }
    if (r < map.size() - 1) {
      if (valid_move(r + 1, c, r, c)) {
        queue.push_back({r + 1, c});
        path_lens[r + 1][c] = cpath + 1;
        // fmt::print("Added ({},{}) with path_len {}\n", r + 1, c, cpath.length + 1);
      }
    }
    if (c > 0) {
      if (valid_move(r, c - 1, r, c)) {
        queue.push_back({r, c - 1});
        path_lens[r][c - 1] = cpath + 1;
        // fmt::print("Added ({},{}) with path_len {}\n", r, c - 1, cpath.length + 1);
      }
    }
    if (c < map[r].size() - 1) {
      if (valid_move(r, c + 1, r, c)) {
        queue.push_back({r, c + 1});
        path_lens[r][c + 1] = cpath + 1;
        // fmt::print("Added ({},{}) with path_len {}\n", r, c + 1, cpath.length + 1);
      }
    }
  }
  return path_lens[goalPos.first][goalPos.second];
}

int main(int argc, char **argv) {
  // std::ifstream file("inp/day12-test.txt");
  std::ifstream file("inp/day12.txt");
  std::string l;

  std::vector<std::string> map;
  while (std::getline(file, l)) {
    map.push_back(l);
  }

  std::deque<std::pair<int, int>> bfs_queue;
  std::deque<std::pair<int, int>> bfs_queue_part2;

  std::pair<int, int> startPos;
  std::pair<int, int> goalPos;
  for (int r = 0; r < map.size(); r++) {
    for (int c = 0; c < map[r].size(); c++) {
      if (map[r][c] == 'S') {
        startPos = {r, c};
        map[r][c] = 'a';
      } else if (map[r][c] == 'E') {
        goalPos = {r, c};
        map[r][c] = 'z';
      }
      if (map[r][c] == 'a') {
        bfs_queue_part2.push_back({r,c});
      }
    }
  }

  bfs_queue.push_back(startPos);
  int part1res = bfs(bfs_queue, map, goalPos);
  fmt::print("Day12: Part1: {}\n", part1res);
  int part2res = bfs(bfs_queue_part2, map, goalPos);
  fmt::print("Day12: Part2: {}\n", part2res);
}
