#pragma once

#include <cstdint>

#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

struct ConstructionTable;

// How the instance at one instantiation is made (LRM 23.3.2): the entry that
// makes an object of the unit elaborated there, and the table that object is
// constructed with in turn. Which unit that is depends on the instance being
// built, never on the code compiled for it, so it is stated where the program
// is assembled and handed to the code.
//
// The entry's prototype is erased so entries of every prototype share one
// array, and a construction restores it to the prototype of the class it
// reads at that instantiation.
struct Instantiation {
  ErasedEntry make_object = nullptr;
  const ConstructionTable* table = nullptr;
};

// What a constructor of the design hierarchy is handed: how the instance at
// each instantiation of the object being built is made, in the order the
// object's unit lays its instantiations out. Every scope of one object is
// constructed with the same table. The tables are constants of the assembled
// program, as a C++ constructor is handed its VTT (Itanium C++ ABI 2.6).
struct ConstructionTable {
  ConstantSpan<Instantiation> instantiations;
};

// The entry and the table of the instantiation at `at` in `table`.
[[nodiscard]] auto InstantiationEntry(
    const ConstructionTable* table, std::int64_t at) -> ErasedEntry;
[[nodiscard]] auto InstantiationTable(
    const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable*;

}  // namespace lyra::runtime
