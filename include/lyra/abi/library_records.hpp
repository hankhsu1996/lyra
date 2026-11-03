#pragma once

#include <cstddef>
#include <type_traits>

#include "lyra/abi/records.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::abi {

// Hands `visit` each structure of the library a unit states constants of, as
// the C++ type the library declares and the library declaration a type naming
// it names. It is the record part of those declarations: a layer stating a
// constant of one reads its members' types and places off the structure handed
// here, so the two sides read one declaration.
template <typename Visit>
constexpr void ForEachLibraryRecord(const Visit& visit) {
  using Decl = support::LibraryDecl;
  visit.template operator()<ObjectDefinition>(Decl::kObjectDefinition);
  visit.template operator()<ScopeInfo>(Decl::kScopeInfo);
  visit.template operator()<ScopeMetadata>(Decl::kScopeMetadata);
  visit.template operator()<ScopeCallable>(Decl::kScopeCallable);
  visit.template operator()<ClassObjectInfo>(Decl::kClassObjectInfo);
  visit.template operator()<InterfaceRow>(Decl::kInterfaceRow);
  visit.template operator()<ConstructionTable>(Decl::kConstructionTable);
  visit.template operator()<Instantiation>(Decl::kInstantiation);
  visit.template operator()<GeneratePlaces>(Decl::kGeneratePlaces);
  visit.template operator()<DumpEntry>(Decl::kDumpEntry);
  visit.template operator()<DumpBlock>(Decl::kDumpBlock);
  visit.template operator()<base::SourceLocation::Impl>(
      Decl::kSourceLocationRecord);
  visit.template operator()<value::Enumeration>(Decl::kEnumeration);
}

// Where `Record` is listed above: the declaration naming it, and how many times
// it is listed, which is once for every structure a constant is stated as.
struct LibraryRecordListing {
  support::LibraryDecl decl{};
  std::size_t times = 0;
};

template <typename Record>
consteval auto ListingOf() -> LibraryRecordListing {
  LibraryRecordListing listing;
  ForEachLibraryRecord([&]<typename Listed>(support::LibraryDecl decl) {
    if constexpr (std::is_same_v<Listed, Record>) {
      listing = {.decl = decl, .times = listing.times + 1};
    }
  });
  return listing;
}

// The library declaration naming the library's structure `Record`.
template <typename Record>
consteval auto LibraryDeclOf() -> support::LibraryDecl {
  static_assert(
      ListingOf<Record>().times == 1,
      "a structure of the library is listed other than once among those a "
      "constant is stated as");
  return ListingOf<Record>().decl;
}

}  // namespace lyra::abi
