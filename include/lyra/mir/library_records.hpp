#pragma once

#include <vector>

#include "lyra/mir/type.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

// A structure of the runtime library a unit states constants of, and the type
// of each of its members in the order the library declares them, as MIR states
// the types: what MIR holds of a record a header declares, as clang holds a
// record declaration it read from one.
struct LibraryRecordDecl {
  support::LibraryDecl decl;
  std::vector<TypeId> members;
};

// Every structure of the library a unit states constants of, each with its
// members' types, read off the library's own declarations.
[[nodiscard]] auto DeclareLibraryRecords(const TypePool& types)
    -> std::vector<LibraryRecordDecl>;

}  // namespace lyra::mir
