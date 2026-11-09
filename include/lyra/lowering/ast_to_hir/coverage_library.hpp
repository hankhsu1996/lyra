#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/component_index.hpp"

// What a covergroup is counted by: Lyra's coverage library (LRM 19), written in
// SystemVerilog and read into a compilation where a design asks for coverage,
// whose classes every covergroup is written over in the step to MIR.
namespace lyra::lowering::ast_to_hir {

// The library's classes, and for each coverpoint of an integral expression the
// integral coverpoint specialized at the coverpoint's type (LRM 19.5.7); a
// coverpoint of a real expression is counted by the real one.
struct CoverageLibrary {
  const slang::ast::CompilationUnitSymbol* unit = nullptr;
  const slang::ast::ClassType* registry = nullptr;
  const slang::ast::ClassType* group_type = nullptr;
  const slang::ast::ClassType* group_instance = nullptr;
  const slang::ast::ClassType* item = nullptr;
  const slang::ast::ClassType* coverpoint = nullptr;
  const slang::ast::ClassType* cross = nullptr;
  const slang::ast::ClassType* selection = nullptr;
  const slang::ast::ClassType* real_coverpoint = nullptr;
  std::unordered_map<
      const slang::ast::CoverpointSymbol*, const slang::ast::ClassType*>
      integral_coverpoints;
};

// Defines what every covergroup of `covergroups` is counted by, once every
// body that could declare one is bound, so each specialization exists before
// the design's units are collected; nothing where the compilation did not read
// the library, which a design that asks for no coverage does not.
auto DefineCoverageClasses(
    slang::ast::Compilation& compilation,
    std::span<const slang::ast::CovergroupType* const> covergroups)
    -> std::optional<CoverageLibrary>;

// The class of the library counting a coverpoint's values.
auto CoverpointClassOf(
    const CoverageLibrary& library, const slang::ast::CoverpointSymbol& point)
    -> const slang::ast::ClassType&;

// The method or the property named `name` of `cls` or of the class it extends
// nearest it, and the class declaring it; nothing where none declares one.
struct LibraryMember {
  const slang::ast::ClassType* owner = nullptr;
  const slang::ast::Symbol* member = nullptr;
};
auto FindLibraryMember(const slang::ast::ClassType& cls, std::string_view name)
    -> std::optional<LibraryMember>;

// The value named `value` of the library's enumeration `enumeration`.
auto LibraryEnumeratorOf(
    const CoverageLibrary& library, std::string_view enumeration,
    std::string_view value) -> const slang::ast::EnumValueSymbol&;

// What an option assignment or an option read is written on (LRM 19.7).
enum class OptionsHolder : std::uint8_t { kCovergroup, kCoverpoint, kCross };

// The place of the member named `name` in the library's structure for the
// options -- `type_option` or `option` -- of what `holder` is.
auto OptionMemberOf(
    const CoverageLibrary& library, OptionsHolder holder, bool type_option,
    std::string_view name) -> base::ComponentIndex;

}  // namespace lyra::lowering::ast_to_hir
