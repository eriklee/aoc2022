#include <set>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <string>
#include <vector>
#include <cassert>

struct Pos {
  int x;
  int y;

  auto operator<=>(const Pos& rhs) const = default;
};

bool adjacent(const Pos& a, const Pos& b) {
  return std::abs(a.x - b.x) <= 1 && std::abs(a.y - b.y) <= 1;
}

template<> struct fmt::formatter<Pos> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) {
    return ctx.end();
  }

  template <typename FormatContext>
  auto format(const Pos& p, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "({},{})", p.x, p.y);
  }
};

enum class Dir { Up, Left, Right, Down, };
enum class DirEx {
  UpLeft, Up, UpRight,
  Left, None, Right,
  DownLeft, Down, DownRight, };

template<> struct fmt::formatter<DirEx> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) {
    return ctx.end();
  }

  template <typename FormatContext>
  auto format(const DirEx& d, FormatContext& ctx) const -> decltype(ctx.out()) {
    switch (d) {
      case DirEx::UpLeft : return fmt::format_to(ctx.out(), "UpLeft");
      case DirEx::Up : return fmt::format_to(ctx.out(), "Up");
      case DirEx::UpRight : return fmt::format_to(ctx.out(), "UpRight");
      case DirEx::Left : return fmt::format_to(ctx.out(), "Left");
      case DirEx::None : return fmt::format_to(ctx.out(), "None");
      case DirEx::Right : return fmt::format_to(ctx.out(), "Right");
      case DirEx::DownLeft : return fmt::format_to(ctx.out(), "DownLeft");
      case DirEx::Down : return fmt::format_to(ctx.out(), "Down");
      case DirEx::DownRight : return fmt::format_to(ctx.out(), "DownRight");
  }
  }
};

DirEx extend(Dir d) {
  switch (d) {
    case Dir::Up : return DirEx::Up;
    case Dir::Left : return DirEx::Left;
    case Dir::Right : return DirEx::Right;
    case Dir::Down : return DirEx::Down;
  }
  __builtin_unreachable();
}

struct Inst {
  Dir direction;
  int count;

  Inst(const std::string& s) {
    if (s[0] == 'U')
      direction = Dir::Up;
    else if (s[0] == 'L')
      direction = Dir::Left;
    else if (s[0] == 'R')
      direction = Dir::Right;
    else if (s[0] == 'D')
      direction = Dir::Down;

    count = std::stoi(s.substr(2));
  }
};

void movePt1(Pos& head, Pos& tail, const Dir dir) {
  if (dir == Dir::Up) {
    head.y++;
    if (tail.y < head.y - 1) {
      tail.y++;
      tail.x = head.x; // may be a noop
    }
  } else if (dir == Dir::Down) {
    head.y--;
    if (tail.y > head.y + 1) {
      tail.y--;
      tail.x = head.x; // may be a noop
    }
  } else if (dir == Dir::Right) {
    head.x++;
    if (tail.x < head.x - 1) {
      tail.x++;
      tail.y = head.y; // may be a noop
    }
  } else if (dir == Dir::Left) {
    head.x--;
    if (tail.x > head.x + 1) {
      tail.x--;
      tail.y = head.y; // may be a noop
    }
  }
}

DirEx change(Pos oldp, Pos newp) {
  if (oldp.x > newp.x) { // left
    if (oldp.y < newp.y) return DirEx::UpLeft;
    if (oldp.y == newp.y) return DirEx::Left;
    if (oldp.y > newp.y) return DirEx::DownLeft;
  } else if (oldp.x == newp.x) {
    if (oldp.y < newp.y) return DirEx::Up;
    if (oldp.y == newp.y) return DirEx::None;
    if (oldp.y > newp.y) return DirEx::Down;
  } else if (oldp.x < newp.x) {
    if (oldp.y < newp.y) return DirEx::UpRight;
    if (oldp.y == newp.y) return DirEx::Right;
    if (oldp.y > newp.y) return DirEx::DownRight;
  }
  __builtin_unreachable();
}

void movePt2(std::vector<Pos>& elems, const Dir dir) {
  // head will always move in this direction
  auto extd = extend(dir);
  movePt1(elems[0], elems[0], dir); // slightly cheating...

  for (int i = 1; i < elems.size(); i++) {
    const auto& head = elems[i - 1];
    auto& tail = elems[i];
    auto prev_tail = tail;

    if (extd == DirEx::Up) {
      if (tail.y < head.y - 1) {
        tail.y++;
        if (tail.x != head.x) {
          tail.x = head.x;
        }
      } 
    } else if (extd == DirEx::Down) {
      if (tail.y > head.y + 1) {
        tail.y--;
        if (tail.x != head.x) {
          tail.x = head.x;
        }
      }
    } else if (extd == DirEx::Right) {
      if (tail.x < head.x - 1) {
        tail.x++;
        if (tail.y != head.y) {
          tail.y = head.y;
        }
      }
    } else if (extd == DirEx::Left) {
      if (tail.x > head.x + 1) {
        tail.x--;
        if (tail.y != head.y) {
          tail.y = head.y;
        }
      }
    } else if (extd == DirEx::None) {
      // chill
    } else if (extd == DirEx::UpLeft) {
      if (!adjacent(tail, head)){
        if (tail.x != head.x)
          tail.x--;
        if (tail.y != head.y)
          tail.y++;
      }
    } else if (extd == DirEx::UpRight) {
      if (!adjacent(tail, head)){
        if (tail.x != head.x)
          tail.x++;
        if (tail.y != head.y)
          tail.y++;
      }
    } else if (extd == DirEx::DownLeft) {
      if (!adjacent(tail, head)){
        if (tail.x != head.x)
          tail.x--;
        if (tail.y != head.y)
          tail.y--;
      }
    } else if (extd == DirEx::DownRight) {
      if (!adjacent(tail, head)){
        if( tail.x != head.x)
          tail.x++;
        if (tail.y != head.y)
          tail.y--;
      }
    }
    extd = change(prev_tail, tail);
    // fmt::print("i:{} prev:{} tail:{} dir:{}\n", i, prev_tail, tail, extd); 
  }
}

int main(int argc, char** argv) {
  auto input = "inp/day9.txt";
  // auto input = "inp/day9_test2.txt";
  std::string l;
  std::set<Pos> visited;

  {
  std::ifstream file(input);
  Pos head = Pos{0,0};
  Pos tail = Pos{0,0};
  visited.insert(tail);

  while (std::getline(file, l)) {
    auto inst = Inst(l);
    for (int i = 0; i < inst.count; i++) {
      movePt1(head, tail, inst.direction);
      //fmt::print("{}, ", tail);
      visited.insert(tail);
    }
    //fmt::print("\n");
  }

  fmt::print("Day9: Part1: {}\n", visited.size());
  }

  // Part2
  std::ifstream file(input);
  visited.clear();
  std::vector<Pos> snake;
  snake.resize(10, Pos{0,0});
  //snake.resize(3, Pos{0,0});

  while (std::getline(file, l)) {
    auto inst = Inst(l);
    // fmt::print("{}:\n", l);
    for (int i = 0; i < inst.count; i++) {
      movePt2(snake, inst.direction);
      //fmt::print("[1] {}, ", snake[1]);
      //fmt::print("[2] {}, ", snake[2]);
      visited.insert(snake[9]);
    }
    // fmt::print("\n");
  }
  fmt::print("Day9: Part2: {}\n", visited.size());
}
