#pragma once

#include <optional>
#include <span>
#include <unordered_map>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/support/imported_runtime_class.hpp"

// What a class of the built-in package `std` is to Lyra. The front end declares
// each with its methods and no body, so something else carries each out: the
// runtime library, for a class whose state only the scheduler holds (LRM 9.7
// `process`), or Lyra's own SystemVerilog, read into every compilation, for one
// a program could have written (LRM 15.3 `semaphore`, 15.4 `mailbox`).
namespace lyra::lowering::ast_to_hir {

// The runtime library class `cls` is, where it is one.
auto ImportedRuntimeClassOf(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass>;

// Whether `unit` is the compilation-unit scope holding Lyra's definitions of
// `std`'s classes.
auto IsStdDefinitions(const slang::ast::CompilationUnitSymbol& unit) -> bool;

// The class defining each class `std` declares and Lyra defines: the class
// itself, and each of its specializations a design names, defined by the
// definition specialized on the same parameters, a `mailbox` of no stated type
// among them.
using StdDefinitions = std::unordered_map<
    const slang::ast::ClassType*, const slang::ast::ClassType*>;

// Defines every class of `std` a design names, once every body that could name
// one is bound, so each specialization of a definition exists before the
// design's units are collected.
auto DefineStdClasses(slang::ast::Compilation& compilation) -> StdDefinitions;

// The classes of Lyra's coverage library (LRM 19), which the same definitions
// hold, that every covergroup is written over in the step to MIR, and for
// each coverpoint of an integral expression the integral coverpoint
// specialized at the coverpoint's type (LRM 19.5.7); a coverpoint of a real
// expression is counted by the real one.
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
// the design's units are collected.
auto DefineCoverageClasses(
    slang::ast::Compilation& compilation,
    std::span<const slang::ast::CovergroupType* const> covergroups)
    -> CoverageLibrary;

// The class of the library counting a coverpoint's values.
auto CoverpointClassOf(
    const CoverageLibrary& library, const slang::ast::CoverpointSymbol& point)
    -> const slang::ast::ClassType&;

// The method of `definition` that `declared`, the front end's declaration of
// one of its methods, stands for.
auto DefiningMethod(
    const slang::ast::ClassType& definition,
    const slang::ast::SubroutineSymbol& declared)
    -> const slang::ast::SubroutineSymbol&;

}  // namespace lyra::lowering::ast_to_hir
