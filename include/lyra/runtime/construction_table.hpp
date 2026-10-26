#pragma once

#include <cstdint>

#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

struct ConstructionTable;

// How the object at one place of a scope's construction is made: one object
// an instantiation stands for (LRM 23.3.2), or a block at a place of a
// generate (LRM 27): the entry that makes an object of the body built there,
// and the table that object is constructed with in turn. Which body that is
// depends on the object being built, never on the code compiled for it, so it
// is stated where the program is assembled and handed to the code.
//
// The entry's prototype is erased so entries of every prototype share one
// array, and a construction restores it to the prototype of the class it
// reads at that place.
struct Instantiation {
  ErasedEntry entry = nullptr;
  const ConstructionTable* table = nullptr;
};

// How the block at each place of one generate is made (LRM 27): a loop's
// places in the order the loop counts its blocks out, as many as it counted
// for the object being built, a conditional's one per alternative the source
// wrote, and a block standing alone at its one place.
struct GeneratePlaces {
  ConstantSpan<Instantiation> places;
};

// What a constructor of a scope of the design hierarchy is handed: how each
// object of each instantiation the scope declares is made, the instantiations
// in the order the scope declares them and each one's objects in row-major
// order, and the places of each of its generates, in the order it declares
// them. A scope's code names only positions of its own table, so it is the
// same code wherever the scope stands. The tables are constants of the
// assembled program.
struct ConstructionTable {
  ConstantSpan<Instantiation> instantiations;
  ConstantSpan<GeneratePlaces> generates;
};

// The entry and the table of the object at `at` among the instantiations of
// `table`.
[[nodiscard]] auto InstantiationEntry(
    const ConstructionTable* table, std::int64_t at) -> ErasedEntry;
[[nodiscard]] auto InstantiationTable(
    const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable*;

// The entry and the table of the block at place `place` of the generate at
// `generate` in `table`.
[[nodiscard]] auto BlockEntry(
    const ConstructionTable* table, std::int64_t generate, std::int64_t place)
    -> ErasedEntry;
[[nodiscard]] auto BlockTable(
    const ConstructionTable* table, std::int64_t generate, std::int64_t place)
    -> const ConstructionTable*;

}  // namespace lyra::runtime
