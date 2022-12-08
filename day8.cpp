#include <cassert>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <string>
#include <vector>

int main(int argc, char **argv) {
  std::ifstream file("inp/day8.txt");
  // std::ifstream file("inp/day8_test.txt");

  std::vector<std::string> input;
  std::string l;

  while (std::getline(file, l)) { input.push_back(std::move(l)); }

  {
    // Part 1
    std::vector<std::vector<bool>> visible_grid;
    // l -> r, r -> l
    for (int ridx = 0; ridx < input.size(); ridx++) {
      const auto& row = input[ridx];
      visible_grid.push_back({});
      visible_grid.back().resize(row.size(), false);

      char max = '0' - 1;
      for (int cidx = 0; cidx < row.size(); cidx++) {
        const auto c = row[cidx];
        if (c > max) {
          visible_grid[ridx][cidx] = true;
          max = c;
        }
      }

      max = '0' - 1;
      for (int cidx = row.size() - 1; cidx >= 0; cidx--) {
        const auto c = row[cidx];
        if (c > max) {
          visible_grid[ridx][cidx] = true;
          max = c;
        }
      }
    }
    // top -> bottom, bottom -> top
    for (int cidx = 0; cidx < input[0].size(); cidx++) {
      char max = '0' - 1;
      for (int ridx = 0; ridx < input.size(); ridx++) {
        const auto& row = input[ridx];
        char c = row[cidx];
        if (c > max) {
          visible_grid[ridx][cidx] = true;
          max = c;
        }
      }
      max = '0' - 1;
      for (int ridx = input.size() - 1; ridx >= 0; ridx--) {
        const auto& row = input[ridx];
        char c = row[cidx];
        if (c > max) {
          visible_grid[ridx][cidx] = true;
          max = c;
        }
      }
    }

    int visible = 0;
    for (const auto &r : visible_grid) {
      for (const auto &b : r) {
        visible += b;
      }
    }
    fmt::print("Day8: Part1: {}\n", visible);
  }
  // End Part 1

  // Part 2
  std::vector<std::vector<int>> scenic_scores;
  std::vector<std::vector<int>> scenic_scores_tmp;

  // go left -> right row by row
  std::vector<int> row_score;
  for (int ridx = 0; ridx < input.size(); ridx++) {
    const auto &row = input[ridx];
    // reset scores
    row_score.clear();
    row_score.push_back(0);

    for (int cidx = 1; cidx < input[0].size(); cidx++) {
      const char c = row[cidx];
      int tmp_cidx = cidx - 1;
      row_score.push_back(1);
      while (tmp_cidx > 0 && c > row[tmp_cidx]) {
        row_score.back() += row_score[tmp_cidx];
        tmp_cidx -= row_score[tmp_cidx];
      }
    }
    scenic_scores.push_back(row_score);

    // fmt::print("done l/r/ ridx={} {}\n", ridx, fmt::join(scenic_scores.back(), ","));

    // reset scores
    std::fill(row_score.begin(), row_score.end(), 1);
    auto max_col = input[0].size() - 1;
    row_score[max_col] = scenic_scores.back()[max_col] = 0;
    for (int cidx = max_col - 1; cidx >= 0; cidx--) {
      const char c = row[cidx];
      int tmp_cidx = cidx + 1;
      auto &score = row_score[cidx];
      while (tmp_cidx < max_col && c > row[tmp_cidx]) {
        score += row_score[tmp_cidx];
        tmp_cidx += row_score[tmp_cidx];
      }
      scenic_scores.back()[cidx] *= row_score[cidx];
    }
    // fmt::print("done r/l/ ridx={} {}\n", ridx, fmt::join(row_score, ","));
  }

  // compute up scores
  // Top row gets scores of 0
  scenic_scores_tmp.push_back({});
  scenic_scores_tmp.back().resize(input[0].size(), 0);
  std::fill(scenic_scores[0].begin(), scenic_scores[0].end(), 0);

  for (int ridx = 1; ridx < input.size(); ridx++) {
    const auto &row = input[ridx];
    scenic_scores_tmp.push_back({});
    scenic_scores_tmp.back().resize(input[0].size(), 1);

    for (int cidx = 0; cidx < row.size(); cidx++) {
      const char c = row[cidx];
      // work back up accumulating scenic-ness from all shorter trees above
      int tmp_ridx = ridx - 1;
      while (tmp_ridx > 0 && c > input[tmp_ridx][cidx]) {
        scenic_scores_tmp[ridx][cidx] += scenic_scores_tmp[tmp_ridx][cidx];
        tmp_ridx -= scenic_scores_tmp[tmp_ridx][cidx];
      }
      scenic_scores[ridx][cidx] *= scenic_scores_tmp.back()[cidx];
    }
    //fmt::print("done t/b ridx={} {}\n", ridx, fmt::join(scenic_scores_tmp.back(), ","));
  }
  // fmt::print("done ups\n");

  // compute down scores
  // bottom row is all 0s
  std::fill(scenic_scores_tmp.back().begin(), scenic_scores_tmp.back().end(),
            0);
  auto max_row = input.size() - 1;
  std::fill(scenic_scores[max_row].begin(), scenic_scores[max_row].end(), 0);
  for (int ridx = max_row - 1; ridx >= 0; ridx--) {
    // reset
    const auto &row = input[ridx];
    std::fill(scenic_scores_tmp[ridx].begin(), scenic_scores_tmp[ridx].end(),
              1);

    for (int cidx = 0; cidx < row.size(); cidx++) {
      const char c = row[cidx];
      // work back up accumulating scenic-ness from all shorter trees above
      int tmp_ridx = ridx + 1;
      while (tmp_ridx < max_row && c > input[tmp_ridx][cidx]) {
        scenic_scores_tmp[ridx][cidx] += scenic_scores_tmp[tmp_ridx][cidx];
        tmp_ridx += scenic_scores_tmp[tmp_ridx][cidx];
      }
      scenic_scores[ridx][cidx] *= scenic_scores_tmp[ridx][cidx];
    }
    // fmt::print("done b/t ridx={} {}\n", ridx, fmt::join(scenic_scores_tmp[ridx], ","));
  }

  // fmt::print("done downs\n");

  int best_score = 0;
  for (const auto &r : scenic_scores) {
    // fmt::print("{}\n", fmt::join(r, ","));
    for (const auto &s : r) {
      if (s > best_score) {
        best_score = s;
        // fmt::print("found new best at {}\n", s);
      }
    }
  }

  fmt::print("Day8: Part2: {}\n", best_score);
}
