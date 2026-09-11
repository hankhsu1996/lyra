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
  kMethod,
  kSynthesizedBody,
  kSynthesizedNamespaceBody,
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
// class's own name is unique only inside its unit while the whole program links
// into one name space, so the unit qualifies it; a member's name is unique only
// inside its class, so the class qualifies that.
auto ClassSymbol(std::string_view unit_name, std::string_view class_name)
    -> std::string;
auto ConstructorSymbol(std::string_view unit_name, std::string_view class_name)
    -> std::string;
auto MethodSymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view method_name) -> std::string;
auto StaticPropertySymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view property_name) -> std::string;

// The symbol a body nothing names is linked under -- one the compiler
// synthesized, which no call site spells. Its position in the arena that holds
// it is its identity, and a category of its own keeps it clear of every symbol
// composed from a name.
auto SynthesizedBodySymbol(
    std::string_view unit_name, std::string_view class_name,
    std::uint32_t ordinal) -> std::string;
auto SynthesizedNamespaceBodySymbol(
    std::string_view unit_name, std::uint32_t ordinal) -> std::string;

// The symbol the runtime record describing one declaration is linked under.
// The record is the compiler's own and stands beside the declaration rather
// than inside it, so it is a category over the same parts.
auto ClassDefinitionSymbol(
    std::string_view unit_name, std::string_view class_name) -> std::string;
auto StructDefinitionSymbol(
    std::string_view unit_name, std::string_view struct_name) -> std::string;
auto ClosureDefinitionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// The symbols of what a unit's namespace owns directly.
auto NamespaceCallableSymbol(
    std::string_view unit_name, std::string_view callable_name) -> std::string;

// The symbols of the two bodies a unit's namespace is brought up through. The
// source declares neither, so neither is composed from a name: a category over
// the unit alone is what lets the design root and the unit that defines them
// arrive at the same symbol with nothing shared between them.
auto NamespaceStorageInstallSymbol(std::string_view unit_name) -> std::string;
auto NamespaceStorageInitializeSymbol(std::string_view unit_name)
    -> std::string;
auto NamespaceVariableSymbol(
    std::string_view unit_name, std::string_view variable_name) -> std::string;
auto StructSymbol(std::string_view unit_name, std::string_view struct_name)
    -> std::string;
auto TypeDescriptionSymbol(
    std::string_view unit_name, std::string_view description_name)
    -> std::string;

// A closure is counted rather than named, having no declaration of the source
// to take a name from; its body is a second symbol over the same ordinal.
auto ClosureSymbol(std::string_view unit_name, std::uint32_t ordinal)
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
