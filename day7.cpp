#include <cassert>
#include <fmt/core.h>
#include <fstream>
#include <memory>
#include <optional>
#include <unordered_map>

struct file {
  file(std::string n, int sz) : name(std::move(n)), size(sz){};
  std::string name;
  int size;
};

struct dir {
  std::string name;
  dir *parent;

  int size = -1;
  std::unordered_map<std::string, file> files;
  std::unordered_map<std::string, std::unique_ptr<dir>> subdirs;
};

void print_fs(const dir &d, int tabs) {
  fmt::print("{:{}}", "", tabs);
  fmt::print("- {} (dir)\n", d.name);
  for (const auto &[_, sd] : d.subdirs) {
    print_fs(*sd, tabs + 1);
  }
  for (const auto &[_, f] : d.files) {
    fmt::print("{:{}}", "", tabs);
    fmt::print("- {} (file, size={})\n", f.name, f.size);
  }
}

std::unique_ptr<dir> build_filesystem() {
  std::ifstream inpfile("inp/day7.txt");
  // std::ifstream inpfile("inp/day7_test.txt");
  std::string l;

  std::getline(inpfile, l);
  assert(l == "$ cd /");
  auto root = std::make_unique<dir>();
  root->name = "/";
  root->parent = nullptr;

  dir *cd = root.get();
  bool have_line = false;

  while (have_line || std::getline(inpfile, l)) {
    assert(l.starts_with('$'));
    // ls
    if (l == "$ ls") {
      while (true) {
        std::getline(inpfile, l);
        if (l.empty() || l[0] == '$')
          break;
        if (l.starts_with("dir ")) {
          auto [it, inserted] =
              cd->subdirs.emplace(l.substr(4), std::make_unique<dir>());
          if (inserted) {
            it->second->name = l.substr(4);
            it->second->parent = cd;
          }
        } else {
          auto space = l.find(' ');
          assert(space != std::string::npos);
          auto fsize = std::stoi(l.substr(0, space));
          auto fname = l.substr(space + 1);
          cd->files.emplace(fname, file(fname, fsize));
        }
      }
      // we've already read the next command
      have_line = !l.empty();
    }
    // cd
    else {
      assert(l.starts_with("$ cd "));
      auto dir = l.substr(5);
      if (dir == "/") {
        cd = root.get();
      } else if (dir == "..") {
        assert(cd->parent);
        cd = cd->parent;
      } else {
        assert(cd->subdirs.contains(dir));
        cd = cd->subdirs.at(dir).get();
      }
      have_line = false;
    }
  }

  return root;
}

int get_dir_sizes(dir &d) {
  int sz = 0;
  for (const auto &[_, sd] : d.subdirs) {
    sz += get_dir_sizes(*sd);
  }
  for (const auto &[_, f] : d.files) {
    sz += f.size;
  }
  d.size = sz;
  return sz;
}

int get_sum_small_sized(const dir &d) {
  int sz = 0;
  for (const auto &[_, sd] : d.subdirs) {
    sz += get_sum_small_sized(*sd);
  }
  if (d.size <= 100000)
    sz += d.size;
  return sz;
}

int get_smallest_dir_gt(const dir &d, const int minSz) {
  if (d.size < minSz)
    return 0;

  int best = d.size;
  for (const auto &[_, sd] : d.subdirs) {
    auto subsize = get_smallest_dir_gt(*sd, minSz);
    if (subsize > 0 && subsize < best)
      best = subsize;
  }
  return best;
}

int main(int argc, char **argv) {
  auto fs = build_filesystem();
  // print_fs(*fs, 0);
  get_dir_sizes(*fs);

  auto part1 = get_sum_small_sized(*fs);
  fmt::print("Day7: Part 1: {}\n", part1);

  auto part2 = get_smallest_dir_gt(*fs, 30000000 - (70000000 - fs->size));
  fmt::print("Day7: Part 2: {}\n", part2);
}
