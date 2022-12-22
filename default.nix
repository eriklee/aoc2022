with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "aoc-2022";
  src = ./.;

  # argh. so clang must go after clang-tools or clangd does not work in vim
  # Thanks random reddit comment flake that pointed this out...
  buildInputs = [ ghc fmt_9 boost180 gdb clang-tools_14 clang_14 bear ];

  # disables FORTIFY_SOURCE, which fails with -O0 and prints ugly output :(
  # https://github.com/NixOS/nixpkgs/issues/18995
  hardeningDisable = [ "fortify" ];
}
