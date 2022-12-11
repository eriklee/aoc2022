#include <cassert>
#include <fmt/core.h>
#include <fstream>
#include <string>
#include <variant>

struct CPU {
  int X = 1;
  int CycleCount = 1;
};

struct Noop {
  static constexpr int CycleCount = 1;
  bool operator==(const Noop &rhs) const = default;
};

struct Addx {
  int IncVal;
  static constexpr int CycleCount = 2;
};

using Inst = std::variant<Noop, Addx>;

Inst parse(const std::string &s) {
  // fmt::print("parsing '{}'\n", s);
  if (s == "noop")
    return {Noop{}};
  assert(s.starts_with("addx "));
  return {Addx{std::stoi(s.substr(5))}};
}

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

int main(int argc, char **argv) {
  // auto input = "inp/day10_test.txt";
  auto input = "inp/day10.txt";
  std::ifstream file(input);
  std::string l;

  CPU cpu;
  int signalSums = 0;
  int retires = 0;
  Inst curInst = {Noop{}};

  
  for (int clock = 1;; clock++) {
    // fmt::print("clock: {} retires: {} signal: {}\n", clock, retires, cpu.X);

    if (retires <= clock) {
      std::visit(overloaded {
          [](Noop) {},
          [&](Addx add) { cpu.X += add.IncVal; }
          }, curInst);
      std::getline(file, l);
      if (l.empty()) break;

      curInst = parse(l);
      retires = clock + std::visit(overloaded {
          [](auto&& arg) { return arg.CycleCount; }
          } , curInst);
    }
    int hpos = (clock - 1) % 40;
    if (clock % 40 == 20 && clock <= 220) {
      // fmt::print("SAVING clock: {} signal: {}\n", clock, cpu.X);
      signalSums += clock * cpu.X;
    }
    bool visible = std::abs(cpu.X - hpos) <= 1;
    fmt::print("{}", visible ? '#' : '.');
    if (clock % 40  == 0) {
      fmt::print("\n");
    }
  }
  fmt::print("Day10: Part1: {}", signalSums);
}
