#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// Which declaration of the program a symbol names. A category is part of the
// symbol rather than a word joined into it, so two declarations of different
// categories never reach one name however they were spelled -- and a category
// the compiler mints, having no source name at all, is nameable without
// borrowing a spelling the source could also write.
enum class SymbolCategory : std::uint8_t {
  kClass,
  kClassDefinition,
  kClosureDefinition,
  kStructDefinition,
  kConstructor,
  kClassCallable,
  kNamespaceCallable,
  kNamespaceStorageInstall,
  kNamespaceStorageInitialize,
  kNamespaceVariable,
  kStaticProperty,
  kClosure,
  kClosureInvoke,
  kStruct,
  kTypeDescription,
};

// One component of a symbol: a name the source wrote, or an ordinal the
// compiler counted. Both self-delimit, so a sequence of them composes to one
// string no other sequence composes to.
struct SymbolPart {
  static auto Name(std::string_view name) -> SymbolPart;
  static auto Ordinal(std::uint32_t value) -> SymbolPart;

  std::string encoded;
};

// The part a declaration contributes to a symbol: the identifier the source
// wrote, or the position the declaration sits at where the source wrote none.
// A part says which kind it is, so the two ranges never meet and a declaration
// the source named can never compose the symbol of one it did not.
auto SymbolPartOf(std::optional<std::string_view> name, std::uint32_t ordinal)
    -> SymbolPart;
auto SymbolPartOf(const std::optional<std::string>& name, std::uint32_t ordinal)
    -> SymbolPart;

// The symbol a declaration is linked under, program-wide.
//
// A SystemVerilog identifier admits every printable character but white space
// (LRM 5.6.1), so no character of it is free to separate one name from the
// next and no word of it is free to mean something the source did not write.
// Each part therefore carries its own extent, and what the compiler mints is a
// category rather than a spelling. The unit that emits a declaration and every
// unit that reaches it build the same parts from the names each already holds,
// so the two agree with no table between them.
auto SymbolName(
    SymbolCategory category, std::initializer_list<SymbolPart> parts)
    -> std::string;

// The symbol a class is linked under, and the symbols of what belongs to it. A
// class's own part is unique only inside its unit while the whole program links
// into one name space, so the unit qualifies it; a member's name is unique only
// inside its class, so the class qualifies that. Every part is a name where the
// source declared one and a position where it did not -- a scope of the design
// hierarchy is a class the lowering built, and a body the lowering synthesized
// is reached by no call site that could spell it.
auto ClassSymbol(std::string_view unit_name, SymbolPart cls) -> std::string;
auto ConstructorSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto ClassCallableSymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart callable)
    -> std::string;
// A class's cell, under the class that owns it. The pool holding a class's
// cells also takes what its bodies keep for the whole class, which the source
// never declared.
auto StaticPropertySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart property)
    -> std::string;

// The symbol the runtime record describing one declaration is linked under.
// The record is the compiler's own and stands beside the declaration rather
// than inside it, so it is a category over the same parts.
auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto StructDefinitionSymbol(std::string_view unit_name, SymbolPart record)
    -> std::string;
auto ClosureDefinitionSymbol(std::string_view unit_name, SymbolPart closure)
    -> std::string;

// The symbols of what a unit's namespace owns directly. A body here always
// answers to the identifier the source declared it under -- what a namespace
// holds that nothing names is storage, never code -- so unlike a class's
// callable this composes from a name and no position arises.
auto NamespaceCallableSymbol(
    std::string_view unit_name, std::string_view callable_name) -> std::string;

// The symbols of the two bodies a unit's namespace is brought up through. The
// source declares neither, so neither is composed from a name: a category over
// the unit alone is what lets the design root and the unit that defines them
// arrive at the same symbol with nothing shared between them.
auto NamespaceStorageInstallSymbol(std::string_view unit_name) -> std::string;
auto NamespaceStorageInitializeSymbol(std::string_view unit_name)
    -> std::string;
// A unit's own storage. A package variable answers to the identifier the source
// declared, which is what another unit reaches it by; the cell a subroutine's
// static-lifetime local keeps answers to none, and takes its position instead.
auto NamespaceVariableSymbol(std::string_view unit_name, SymbolPart variable)
    -> std::string;

// A gathered-scope aggregate, which the source never declared, so its position
// in the unit's registry is the whole of its identity.
auto StructSymbol(std::string_view unit_name, SymbolPart record) -> std::string;
// The run-time description of one of a unit's types. The source declares no
// such thing, so the position the type sits at in its unit's pool is the whole
// of what identifies it.
auto TypeDescriptionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// A closure is counted rather than named, having no declaration of the source
// to take a name from; its body is a second symbol over the same ordinal.
auto ClosureSymbol(std::string_view unit_name, SymbolPart closure)
    -> std::string;
auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// The symbol the declaration `type` names is linked under, or nothing where the
// type names no declaration a value is built from. A declaration this unit
// compiles carries the name it was emitted under; one another unit declares is
// composed from the unit and the name a signature gave, the same way that unit
// composed it -- which is what lets the two agree with no shared table.
auto DeclarationSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

// The symbol the runtime record describing `type`'s declaration is linked
// under. The record is the compiler's own, so it is a category of its own
// rather than a word appended to the declaration's symbol.
auto DefinitionSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

}  // namespace lyra::lir
