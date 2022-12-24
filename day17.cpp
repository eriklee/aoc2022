#include <cassert>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fstream>
#include <optional>
#include <string>
#include <vector>
#include <array>
#include <boost/container_hash/hash.hpp>
#include <boost/container/flat_map.hpp>

/*
 * each row represented by a single byte
 * pieces are 4 bytes long
 * left and right shifts are simply bit shifts (and some checking)
 * checking: 
 *  left shift: MSB in each byte should always be 0
 *  right: don't shift if LSB is ever 1
 * collision checking = &
 * just keep pushing bytes onto the back as necessary
 * */


using TPiece = std::array<uint8_t, 4>;

static constexpr TPiece piece1 = {0b00011110, 0x00, 0x00, 0x00};
static constexpr TPiece piece2 = {0b00001000, 0b00011100, 0b00001000, 0x00};
static constexpr TPiece piece3 = {0b00011100, 0b00000100, 0b00000100, 0x00};
static constexpr TPiece piece4 = {0b00010000, 0b00010000,0b00010000,0b00010000,};
static constexpr TPiece piece5 = {0b00011000, 0b00011000, 0x00, 0x00};

static constexpr TPiece pieces[5] = {piece1, piece2, piece3, piece4, piece5};
static constexpr int pieceHeights[5] = {1, 3, 3, 4, 2};

// If & with piece produces a non-zero it's already against the right edge
static constexpr TPiece kCanShiftRightMask = { 0x01, 0x01, 0x01, 0x01 };

// If & with pieces produces a non-zero it's already against the left edge
static constexpr TPiece kCanShiftLeftMask = { 0x40, 0x40, 0x40, 0x40 };

// Shifts the piece to the right - returning the shifted version
// returns whether it was successful or not
bool shiftRight(const std::array<uint8_t,4> piece, std::array<uint8_t, 4>& out) {
  bool blocked = false;
  for (int i = 0; i < 4; i++) {
    out[i] = piece[i];
    blocked |= (kCanShiftRightMask[i] & piece[i]) != 0;
  }
  if (blocked) return false;
  for (int i = 0; i < 4; i++) {
    out[i] = out[i] >> 1;
  }

  return true;
}

bool shiftLeft(const std::array<uint8_t,4> piece, std::array<uint8_t, 4>& out) {
  bool blocked = false;
  for (int i = 0; i < 4; i++) {
    out[i] = piece[i];
    blocked |= (kCanShiftLeftMask[i] & piece[i]) != 0;
  }
  if (blocked) return false;
  for (int i = 0; i < 4; i++) {
    out[i] = out[i] << 1;
  }

  return true;
}

bool collides(const TPiece& piece, const TPiece& mapFrag) {
  bool result = false;
  for (int i = 0; i < 4; i++) {
    result |= (piece[i] & mapFrag[i]) != 0;
  }
  return result;
}

void placePiece(const TPiece& piece, TPiece* mapFrag) {
  for (int i = 0; i < 4; i++) {
    (*mapFrag)[i] |= piece[i];
  }
}

// returns the level at which the piece stopped (the bottom)
uint64_t dropPiece(const std::vector<uint8_t>& map, const std::string& wind, TPiece& piece, uint64_t& windex, uint64_t top) {
  TPiece shifted;
  while (top > 0) {
    // move with the jet
    auto didShift = wind[windex++] == '<' ? shiftLeft(piece, shifted) : shiftRight(piece, shifted);
    (void) didShift;
    if (!collides(shifted, *reinterpret_cast<const TPiece*>(map.data() + top))) {
      piece = shifted;
    }
    if (windex == wind.size()) windex = 0;
    
    // see if we can move down
    if (collides(piece, *reinterpret_cast<const TPiece*>(map.data() + --top))) {
      return top + 1;
    }
  }
  return top;
}

void printMap(const std::vector<uint8_t>& map, int top, uint64_t top_total) {
  int i = top + 3;
  // for (; i > top - 10 && i > 0; i--) {
  for (; i >  0; i--) {
    fmt::print("\n |");
    auto row = map[i];
    for (int i = 6; i >= 0; i--) {
      fmt::print("{}", ((row >> i) & 1) == 0 ? "." : "#");
    }
    fmt::print("|");
  }
  fmt:: print("  {}\n", top_total);
  fmt:: print(" +-------+\n");
}

uint64_t hashState(const std::vector<uint8_t>& map, uint64_t top, int pieceIdx, int windex) {
  size_t result = 0xA0CF2022;
  boost::hash_combine(result, pieceIdx);
  boost::hash_combine(result, windex);
  for (int i = top - 20; i < top; i++) {
    boost::hash_combine(result, map[i]);
  }
  return result;
}

int main(int argc, char **argv) {
  // std::ifstream file("inp/day17-test.txt");
  std::ifstream file("inp/day17.txt");
  std::string wind;
  std::getline(file, wind);

  std::vector<uint8_t> map;
  map.push_back(0xff); // stick a row on the bottom to simplify the checks
  uint64_t top = 1;
  uint64_t top_total = 0;
  uint64_t windex = 0;

  for (int pidx = 0; pidx < 2022; pidx++) {
    map.resize(top + 7);
    if (map.size() > 512) {
      auto diff = map.size() - 256;
      top_total += diff;
      //fmt::print("Rolling: sz: {} diff: {} top: {} top_total:{}", map.size(), diff, top, top_total);
      std::copy(map.end() - 256, map.end(), map.begin());
      std::fill(map.begin() + 256, map.end(), 0);
      top -= diff;
      //fmt::print(" after: top: {} sz: {}\n", top, map.size());
    }
    TPiece piece = pieces[pidx % 5];
    auto bottom = dropPiece(map, wind, piece, windex, top + 3);
    placePiece(piece, reinterpret_cast<TPiece*>(map.data() + bottom));

    top = std::max(top, bottom + pieceHeights[pidx % 5]);
    // printMap(map, top);
  }
  
  // printMap(map, top, top_total);

  //fmt::print("top_total:{} top:{}\n", top_total , top ); // remember the extra full row 0
  fmt::print("Day17: Part1: {}\n", top_total + top - 1); // remember the extra full row 0

  struct LTState {
    uint64_t pIdx;
    uint64_t windex;
    uint64_t pieceType;
    uint64_t value;
  };
  boost::container::flat_map<uint64_t, LTState> hashes;

  uint64_t pidx = 2022;
  static constexpr uint64_t totalRocks = 1000000000000;
  bool cycleFound = false;
  for (; pidx < totalRocks; pidx++) {
    map.resize(top + 7);

    map.resize(top + 7);
    if (map.size() > 512) {
      auto diff = map.size() - 256;
      top_total += diff;
      //fmt::print("Rolling: pidx: {} sz: {} diff: {} top: {} top_total:{}", pidx, map.size(), diff, top, top_total);
      std::copy(map.end() - 256, map.end(), map.begin());
      std::fill(map.begin() + 256, map.end(), 0);
      top -= diff;
      //fmt::print(" after: top: {} sz: {}\n", top, map.size());
    }

    TPiece piece = pieces[pidx % 5];
    auto bottom = dropPiece(map, wind, piece, windex, top + 3);
    placePiece(piece, reinterpret_cast<TPiece*>(map.data() + bottom));

    top = std::max(top, bottom + pieceHeights[pidx % 5]);
    auto hash = hashState(map, top, pidx % 5, windex);
    
    if (!cycleFound && hashes.contains(hash)) {
      // auto lastSt = hashes[hash];
      // fmt::print("duplicate found: {}\n", hash);
      // fmt::print("last_st: pidx:{} windex:{} pt:{} val:{}\n", lastSt.pIdx, lastSt.windex, lastSt.pieceType, lastSt.value);
      // fmt::print("state: pidx:{} windex:{} pt:{} val:{}\n", pidx, windex, pidx % 5, top_total + top - 1);
      auto cycle_size = (top + top_total - 1) - hashes[hash].value;
      auto cycle_len = pidx - hashes[hash].pIdx;
      // fmt::print("cycle_info: sz: {} len: {}\n", cycle_size, cycle_len);
      uint64_t cycles_left = (totalRocks - pidx) / cycle_len;
      top_total += cycles_left * cycle_size;
      // skip ahead to the last cycle now
      pidx += (cycles_left * cycle_len);
      cycleFound = true;
    } else {
      hashes[hash] = {pidx, windex, pidx % 5, top_total + top - 1};
    }
  }
  fmt::print("Day17: Part2: {}\n", top_total + top - 1); // remember the extra full row 0
}

