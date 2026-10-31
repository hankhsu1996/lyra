#pragma once

#include <cstddef>
#include <type_traits>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

// A structure of the runtime library a unit states constants of, and the type
// of each of its members in the order the library declares them, as MIR states
// the types: what MIR holds of a record a header declares, as clang holds a
// record declaration it read from one.
struct LibraryRecordDecl {
  RuntimeLibraryKind kind;
  std::vector<TypeId> members;
};

// Hands `visit` each structure of the library a unit states constants of, as
// the C++ type the library declares and the kind MIR names it by.
template <typename Visit>
constexpr void ForEachLibraryRecord(const Visit& visit) {
  using Kind = RuntimeLibraryKind;
  visit.template operator()<abi::ObjectDefinition>(Kind::kObjectDefinition);
  visit.template operator()<abi::ScopeInfo>(Kind::kScopeInfo);
  visit.template operator()<abi::ScopeMetadata>(Kind::kScopeMetadata);
  visit.template operator()<abi::ScopeCallable>(Kind::kScopeCallable);
  visit.template operator()<abi::ClassObjectInfo>(Kind::kClassObjectInfo);
  visit.template operator()<abi::InterfaceRow>(Kind::kInterfaceRow);
  visit.template operator()<abi::ConstructionTable>(Kind::kConstructionTable);
  visit.template operator()<abi::Instantiation>(Kind::kInstantiation);
  visit.template operator()<abi::GeneratePlaces>(Kind::kGeneratePlaces);
  visit.template operator()<abi::DumpEntry>(Kind::kDumpEntry);
  visit.template operator()<abi::DumpBlock>(Kind::kDumpBlock);
}

// Where `Record` is listed above: the kind it is named by, and how many times
// it is listed, which is once for every structure a constant is stated as.
struct LibraryRecordListing {
  RuntimeLibraryKind kind{};
  std::size_t times = 0;
};

template <typename Record>
consteval auto ListingOf() -> LibraryRecordListing {
  LibraryRecordListing listing;
  ForEachLibraryRecord([&]<typename Listed>(RuntimeLibraryKind kind) {
    if constexpr (std::is_same_v<Listed, Record>) {
      listing = {.kind = kind, .times = listing.times + 1};
    }
  });
  return listing;
}

// The kind MIR names the library's structure `Record` by.
template <typename Record>
consteval auto LibraryRecordKind() -> RuntimeLibraryKind {
  static_assert(
      ListingOf<Record>().times == 1,
      "a structure of the library is listed other than once among those a "
      "constant is stated as");
  return ListingOf<Record>().kind;
}

// Every structure of the library a unit states constants of, each with its
// members' types, read off the library's own declarations.
[[nodiscard]] auto DeclareLibraryRecords(const TypePool& types)
    -> std::vector<LibraryRecordDecl>;

}  // namespace lyra::mir
