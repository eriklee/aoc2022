#include <cassert>
#include <charconv>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <string>
#include <vector>
#include <functional>

// This is slightly annoying
struct Lameda {
  char op;
  uint64_t rhs = 0;
  uint64_t eval(uint64_t lhs) {
    if (rhs <= 0) {
      if (op == '+') return lhs + lhs;
      else  return lhs * lhs;
    } else {
      if (op == '+') return lhs + rhs;
      else  return lhs * rhs;
    }
  }
};

struct Monkey {
  std::vector<uint64_t> items;
  Lameda op;
  uint64_t divisor;
  uint64_t trueTarget;
  uint64_t falseTarget;
  
  void toss(uint64_t item) { items.push_back(item); }
  uint64_t target(uint64_t item) { return (item % divisor == 0) ? trueTarget : falseTarget; }
};

Lameda parseOp(const std::string& l) {
  assert(l.starts_with("  Operation: new = "));
  auto formula = l.substr(std::string("  Operation: new = ").size());

  assert(formula.starts_with("old "));
  formula = formula.substr(4);
  char op = formula[0];
  assert(op == '+' || op == '*');
  formula = formula.substr(2);
  uint64_t rhs = -1;
  auto fcres = std::from_chars(formula.data(), formula.data() + formula.size(), rhs);
  if (fcres.ec != std::errc()) {
    assert(formula == "old");
    return Lameda{op};
  } else {
    return Lameda{op, rhs};
  }
}

std::vector<Monkey> parseMonkeys(std::string infile) {
  std::vector<Monkey> result;

  std::ifstream file(infile);
  std::string l;
  
  while (!file.eof()) {
    std::getline(file, l);
    assert(l.starts_with("Monkey "));
    std::getline(file, l);
    assert(l.starts_with("  Starting items: "));
    std::vector<uint64_t> items;
    const char* cstr = l.c_str();
    cstr += strlen("  Starting items: ");
    uint64_t n;
    while (strlen(cstr)) {
        if (*cstr == ',') cstr+=2;
        auto res = std::from_chars(cstr, cstr + strlen(cstr), n);
        assert(res.ec == std::errc());
        cstr = res.ptr;
        items.push_back(n);
    };
    std::getline(file, l);
    assert(l.starts_with("  Operation: new = "));
    auto f = parseOp(l);

    std::getline(file, l);
    assert(l.starts_with("  Test: divisible by "));
    l = l.substr(std::string("  Test: divisible by ").size());
    uint64_t divisor;
    auto res = std::from_chars(l.data(), l.data() + l.size(), divisor);
    assert(res.ec == std::errc());

    std::getline(file, l);
    assert(l.starts_with("    If true: throw to monkey "));
    l = l.substr(std::string("    If true: throw to monkey ").size());
    uint64_t trueTarget;
    res = std::from_chars(l.data(), l.data() + l.size(), trueTarget);
    assert(res.ec == std::errc());

    std::getline(file, l);
    assert(l.starts_with("    If false: throw to monkey "));
    l = l.substr(std::string("    If false: throw to monkey ").size());
    uint64_t falseTarget;
    res = std::from_chars(l.data(), l.data() + l.size(), falseTarget);
    assert(res.ec == std::errc());

    // fmt::print("monkey {}: [{}], /{}, tt:{} ft:{}\n", result.size(), fmt::join(items, ","), divisor, trueTarget, falseTarget);

    result.emplace_back(Monkey{std::move(items), std::move(f), divisor, trueTarget, falseTarget});
    std::getline(file, l); // blank line after, maybe
  }
  return result;
}

uint64_t monkeyRound(std::vector<Monkey>& monkeys, uint64_t idx) {
  // fmt::print("Monkey {}:\n", idx);
  auto& monkey = monkeys.at(idx);
  uint64_t res = monkey.items.size();
  for (auto item : monkey.items) {
    // fmt::print("\tMonkey inpects an item with a worry level of {}.\n", item);
    item = monkey.op.eval(item);
    // fmt::print("\tWorry level goes to {}.\n", item);
    item /= 3;
    // fmt::print("\tWorry level divided by 3 -> {}.\n", item);
    auto target = monkey.target(item);
    // fmt::print("\titem is thrown to monkey {}.\n", target);
    monkeys.at(target).toss(item);
  }
  monkey.items.clear();

  return res;
}

uint64_t monkeyRound2(std::vector<Monkey>& monkeys, const uint64_t idx, const uint64_t worryDivisor) {
  auto& monkey = monkeys.at(idx);
  uint64_t res = monkey.items.size();
  for (auto item : monkey.items) {
    item = monkey.op.eval(item);
    item %= worryDivisor;
    auto target = monkey.target(item);
    monkeys.at(target).toss(item);
  }
  monkey.items.clear();

  return res;
}

int main(int argc, char** argv) {
  // auto monkeys = parseMonkeys("inp/day11_test.txt");
  auto monkeys = parseMonkeys("inp/day11.txt");
  auto monkeys2 = monkeys;
  std::vector<uint64_t> monkeyInspections;
  monkeyInspections.resize(monkeys.size(), 0);

  for (int round = 0; round < 20; round++) {
    for (int midx = 0; midx < monkeys.size(); midx++) {
      monkeyInspections[midx] += monkeyRound(monkeys, midx);
    }
  }
  std::sort(monkeyInspections.begin(), monkeyInspections.end());
  uint64_t res = monkeyInspections[monkeyInspections.size() - 1] * monkeyInspections[monkeyInspections.size() - 2];
  fmt::print("Day11: Part1: {}\n", res);

  uint64_t worryDivisor = 1;
  monkeys.clear();

  for (const auto& m : monkeys2) { worryDivisor *= m.divisor; }

  std::fill(monkeyInspections.begin(), monkeyInspections.end(), 0);
  for (int round = 0; round < 10000; round++) {
    for (int midx = 0; midx < monkeys2.size(); midx++) {
      monkeyInspections[midx] += monkeyRound2(monkeys2, midx, worryDivisor);
    }
  }
  std::sort(monkeyInspections.begin(), monkeyInspections.end());
  res = monkeyInspections[monkeyInspections.size() - 1] * monkeyInspections[monkeyInspections.size() - 2];
  fmt::print("Day11: Part2: {}\n", res);
}
