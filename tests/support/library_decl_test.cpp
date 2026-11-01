#include "lyra/support/library_decl.hpp"

#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <set>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/abi/library_records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/movability.hpp"

namespace lyra::support {
namespace {

auto EveryDecl() -> std::vector<LibraryDecl> {
  std::vector<LibraryDecl> all;
  ForEachLibraryDecl([&](LibraryDecl decl) { all.push_back(decl); });
  return all;
}

// Each declaration's facts are read by several layers apart, so what one says
// has to agree with what another says of the same declaration; these are the
// agreements no single reader checks.
TEST(LibraryDeclTest, EachDeclarationStatesAConsistentType) {
  const std::vector<LibraryDecl> all = EveryDecl();
  ASSERT_FALSE(all.empty());
  std::set<std::string_view> names;
  for (const LibraryDecl decl : all) {
    const LibraryDeclData& d = DeclarationOf(decl);
    SCOPED_TRACE(d.name);
    EXPECT_TRUE(names.insert(d.name).second) << "two declarations share a name";
    EXPECT_FALSE(d.cpp_spelling.empty());
    // An object of a class of the runtime's and a keeper are each kept where
    // they were made.
    if (d.runtime_class.has_value() || d.keeper.has_value()) {
      EXPECT_TRUE(IsAddressOnly(decl));
    }
    // A spelling written over the arguments needs arguments to write.
    if (d.cpp_spelling_takes_args) {
      EXPECT_FALSE(d.params.empty());
    }
    // What one starts as is a value a temporary can stand in for, built by a
    // call rather than a construction, which reaches the one constructor a
    // type has.
    if (d.default_value.has_value()) {
      EXPECT_NE(d.movability, Movability::kImmovable);
      EXPECT_FALSE(
          std::holds_alternative<Constructor>(
              RuntimeEntryOf(*d.default_value).declaration));
    }
    // A data type is a value its holder keeps whole, which a copy duplicates
    // and a declared variable starts as something (LRM 6.2, Table 6-7).
    if (d.is_a_data_type) {
      EXPECT_TRUE(IsRuntimeStoredValue(decl));
      EXPECT_EQ(d.movability, Movability::kCopyable);
      EXPECT_TRUE(d.default_value.has_value());
    }
    // What a construction reaches is a constructor, which no call reaches.
    if (d.constructor.has_value()) {
      EXPECT_TRUE(
          std::holds_alternative<Constructor>(
              RuntimeEntryOf(d.constructor->entry).declaration));
      if (d.constructor->integral_entry.has_value()) {
        EXPECT_TRUE(
            std::holds_alternative<Constructor>(
                RuntimeEntryOf(*d.constructor->integral_entry).declaration));
      }
    }
  }
}

// A container sized while the program runs states its element type among its
// arguments, so the rules the language gives containers read it there; it is
// a data type (LRM 7.5, 7.8, 7.10), and a bound is a queue's alone.
TEST(LibraryDeclTest, AContainerStatesItsElementAmongItsArguments) {
  std::set<LibraryDecl> containers;
  for (const LibraryDecl decl : EveryDecl()) {
    const LibraryDeclData& d = DeclarationOf(decl);
    SCOPED_TRACE(d.name);
    const std::optional<std::size_t> element =
        ParamOf(decl, LibraryParam::kElement);
    const std::optional<std::size_t> index =
        ParamOf(decl, LibraryParam::kIndex);
    if (!element.has_value()) {
      EXPECT_FALSE(index.has_value());
      EXPECT_FALSE(ParamOf(decl, LibraryParam::kBound).has_value());
      continue;
    }
    containers.insert(decl);
    EXPECT_TRUE(d.is_a_data_type);
  }
  EXPECT_EQ(
      containers, (std::set<LibraryDecl>{
                      LibraryDecl::kDynamicArray, LibraryDecl::kQueue,
                      LibraryDecl::kAssociativeArray}));
  EXPECT_EQ(ParamOf(LibraryDecl::kQueue, LibraryParam::kElement), 0U);
  EXPECT_EQ(ParamOf(LibraryDecl::kQueue, LibraryParam::kBound), 1U);
  EXPECT_EQ(ParamOf(LibraryDecl::kAssociativeArray, LibraryParam::kIndex), 0U);
  EXPECT_EQ(
      ParamOf(LibraryDecl::kAssociativeArray, LibraryParam::kElement), 1U);
}

// A string and the containers are what a variable may be declared to hold
// (LRM 6.2); a value of any type is held only by the mailbox it is a message
// of (LRM 15.4.9), and nothing else the library declares is a data type.
TEST(LibraryDeclTest, TheDataTypesAreTheStringAndTheContainers) {
  std::set<LibraryDecl> data_types;
  for (const LibraryDecl decl : EveryDecl()) {
    if (DeclarationOf(decl).is_a_data_type) {
      data_types.insert(decl);
    }
  }
  EXPECT_EQ(
      data_types, (std::set<LibraryDecl>{
                      LibraryDecl::kString, LibraryDecl::kDynamicArray,
                      LibraryDecl::kQueue, LibraryDecl::kAssociativeArray}));
}

// Every constructor is named by a declaration, directly or as the one it
// reaches over an integral value, so none is left that nothing reaches.
TEST(LibraryDeclTest, EveryConstructorIsReachedThroughADeclaration) {
  std::set<BuiltinFn> named;
  for (const LibraryDecl decl : EveryDecl()) {
    const std::optional<LibraryConstructor> constructor =
        DeclarationOf(decl).constructor;
    if (!constructor.has_value()) {
      continue;
    }
    named.insert(constructor->entry);
    if (constructor->integral_entry.has_value()) {
      named.insert(*constructor->integral_entry);
    }
  }
  const auto declared = [](BuiltinFn fn) -> std::optional<RuntimeEntry> {
    try {
      return RuntimeEntryOf(fn);
    } catch (const InternalError&) {
      return std::nullopt;
    }
  };
  for (std::uint32_t i = 0;; ++i) {
    const auto fn = static_cast<BuiltinFn>(i);
    const std::optional<RuntimeEntry> entry = declared(fn);
    if (!entry.has_value()) {
      break;
    }
    if (std::holds_alternative<Constructor>(entry->declaration)) {
      EXPECT_TRUE(named.contains(fn)) << entry->name;
    }
  }
  EXPECT_FALSE(named.empty());
}

// A structure a unit states as a constant is data the run reads where it
// lies, so a frame names one by its address, and no two structures are one
// declaration.
TEST(LibraryDeclTest, EachRecordIsADeclarationNamedByItsAddress) {
  std::set<LibraryDecl> listed;
  abi::ForEachLibraryRecord([&]<typename Record>(LibraryDecl decl) {
    EXPECT_TRUE(listed.insert(decl).second);
    EXPECT_TRUE(
        std::holds_alternative<StorageElsewhere>(DeclarationOf(decl).holding));
  });
  EXPECT_FALSE(listed.empty());
}

}  // namespace
}  // namespace lyra::support
