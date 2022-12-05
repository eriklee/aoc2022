with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "aoc-2022";
  src = ./.;

  buildInputs = [ ghc gcc_latest fmt_9 gdb clang_14 clang-tools_14 ];

}
