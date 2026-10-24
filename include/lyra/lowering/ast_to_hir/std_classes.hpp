#pragma once

#include <optional>
#include <unordered_map>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"

// What a class of the built-in package `std` is to Lyra. The front end declares
// each with its methods and no body, so something else carries each out: the
// runtime library, for a class whose state only the scheduler holds (LRM 9.7
// `process`), or Lyra's own SystemVerilog, read into every compilation, for one
// a program could have written (LRM 8.30 `weak_reference`, 15.3 `semaphore`,
// 15.4 `mailbox`). A class defined in SystemVerilog may still have a method
// only the runtime can answer -- an object's identity (LRM 8.30.5) -- which the
// runtime carries out as it does every method of a runtime class.
namespace lyra::lowering::ast_to_hir {

// The runtime library class `cls` is, where it is one.
auto ImportedRuntimeClassOf(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass>;

// The library entry carrying out `method`, a method the front end declares in
// `std`, where the runtime carries it out rather than Lyra's definitions.
auto RuntimeCarriedMethodOf(const slang::ast::SubroutineSymbol& method)
    -> std::optional<support::BuiltinFn>;

// Whether `prop` refers to the object its handle names without keeping it
// (LRM 8.30): a property Lyra's own definitions declare `(* lyra_weak *)`. The
// attribute means nothing in a design's own text, which LRM 5.12 leaves to the
// tool.
auto IsHeldWeakly(const slang::ast::ClassPropertySymbol& prop) -> bool;

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

// The method of `definition` that `declared`, the front end's declaration of
// one of its methods, stands for.
auto DefiningMethod(
    const slang::ast::ClassType& definition,
    const slang::ast::SubroutineSymbol& declared)
    -> const slang::ast::SubroutineSymbol&;

}  // namespace lyra::lowering::ast_to_hir
