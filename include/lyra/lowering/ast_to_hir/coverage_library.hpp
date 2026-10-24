#pragma once

#include <optional>
#include <span>
#include <unordered_map>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>

// What a covergroup is counted by: Lyra's coverage library (LRM 19), written in
// SystemVerilog and read into a compilation where a design asks for coverage,
// whose classes every covergroup is written over in the step to MIR.
namespace lyra::lowering::ast_to_hir {

// The library's classes, and for each coverpoint of an integral expression the
// integral coverpoint specialized at the coverpoint's type (LRM 19.5.7); a
// coverpoint of a real expression is counted by the real one.
struct CoverageLibrary {
  const slang::ast::ClassType* registry = nullptr;
  const slang::ast::ClassType* group_type = nullptr;
  const slang::ast::ClassType* group_instance = nullptr;
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

}  // namespace lyra::lowering::ast_to_hir
