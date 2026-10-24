#pragma once

#include <optional>
#include <string_view>

#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/text/SourceLocation.h>

// The texts of SystemVerilog Lyra reads into a compilation beside the design's
// sources, each read as a compilation unit of its own (LRM 3.12.1).
namespace lyra::frontend {

// One of those texts: the buffer it is read as, which tells a unit read from
// it apart, and the name of the unit its scope is compiled as. No source can
// write either, so neither meets a name of the design's.
struct LibraryText {
  std::string_view buffer;
  std::string_view unit;
};

// Lyra's definitions of the classes the front end declares in the built-in
// package `std` (LRM 15.3, 15.4), read into every compilation and compiled in
// the name of `std`, the unit a name of one of them reaches.
inline constexpr LibraryText kStdDefinitions{.buffer = "<std>", .unit = "std"};

// What a covergroup is counted by (LRM 19), read where a design asks for
// coverage.
inline constexpr LibraryText kCoverageLibrary{
    .buffer = "<coverage>", .unit = "$unit__coverage"};

// The buffer the members of `unit` were read from, where it has a located
// member.
auto BufferOf(const slang::ast::CompilationUnitSymbol& unit)
    -> std::optional<slang::BufferID>;

// The text of Lyra's own `unit` was read from, or none where `unit` is the
// design's.
auto LibraryTextOf(const slang::ast::CompilationUnitSymbol& unit)
    -> const LibraryText*;

}  // namespace lyra::frontend
