#pragma once

#include <cstdint>

#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

struct ConstructionTable;

// How the instance at one instantiation is made (LRM 23.3.2): the entry that
// makes an object of the body elaborated there, and the table that object is
// constructed with in turn. Which body that is depends on the instance being
// built, never on the code compiled for it, so it is stated where the program
// is assembled and handed to the code.
//
// The entry's prototype is erased so entries of every prototype share one
// array, and a construction restores it to the prototype of the class it
// reads at that instantiation.
struct Instantiation {
  ErasedEntry entry = nullptr;
  const ConstructionTable* table = nullptr;
};

// What a constructor of a scope of the design hierarchy is handed: how the
// instance at each instantiation the scope declares is made, in the order the
// scope declares them, and the table each of its generate blocks is
// constructed with in turn, generate by generate. A scope's code names only
// positions of its own table, so it is the same code wherever the scope
// stands. The tables are constants of the assembled program.
struct ConstructionTable {
  ConstantSpan<Instantiation> instantiations;
  ConstantSpan<const ConstructionTable*> blocks;
};

// The entry and the table of the instantiation at `at` in `table`.
[[nodiscard]] auto InstantiationEntry(
    const ConstructionTable* table, std::int64_t at) -> ErasedEntry;
[[nodiscard]] auto InstantiationTable(
    const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable*;

// The table the block at `at` among the blocks of the scope `table` was handed
// to is constructed with.
[[nodiscard]] auto BlockTable(const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable*;

}  // namespace lyra::runtime
