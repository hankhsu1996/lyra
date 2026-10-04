#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {

// Which declaration of the program a symbol names. A category is part of the
// symbol rather than a word joined into it, so two declarations of different
// categories never reach one name however they were spelled -- and a category
// the compiler mints, having no source name at all, is nameable without
// borrowing a spelling the source could also write.
enum class SymbolCategory : std::uint8_t {
  kClassDefinition,
  kClosureDefinition,
  kConstructor,
  kClassCallable,
  kStructMethod,
  kNamespaceCallable,
  kNamespaceStorageInstall,
  kNamespaceStorageInitialize,
  kObjectEntry,
  kNamespaceVariable,
  kStaticProperty,
  kClosureInvoke,
  kTypeDescription,
  kIntegralConstant,
  kConstructorPrologue,
  kBaseObjectDestructor,
  kCompleteObjectDestructor,
  kDeletingDestructor,
  kClassConstant,
  kDispatchTable,
  kTypeInfo,
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

// The symbols of what belongs to a class. A class's own part is unique only
// inside its unit while the whole program links into one name space, so the
// unit qualifies it; a member's name is unique only inside its class, so the
// class qualifies that. Every part is a name where the source declared one and
// a position where it did not -- a scope of the design hierarchy is a class the
// lowering built, and a body the lowering synthesized is reached by no call
// site that could spell it.
auto ConstructorSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto ClassCallableSymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart callable)
    -> std::string;
// A struct's method (LRM 7.2), under the struct the unit declares. The
// operation it answers is its name, since a struct has one method for each.
auto StructMethodSymbol(
    std::string_view unit_name, std::string_view structure,
    support::ValueOperation operation) -> std::string;
// A class's cell, under the class that owns it. The pool holding a class's
// cells also takes what its bodies keep for the whole class, which the source
// never declared.
auto StaticPropertySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart property)
    -> std::string;

// The symbols of what a unit's namespace owns directly. A body the source
// declared answers to the identifier it declared it under, which is what
// another unit reaches it by; a body the compiler synthesized into the
// namespace answers to none and takes its position instead, exactly as a
// class's own bodies and as this namespace's storage already do.
auto NamespaceCallableSymbol(std::string_view unit_name, SymbolPart callable)
    -> std::string;

// The symbols of the bodies a unit publishes that the source declares none of:
// the two its namespace is brought up through, and the one that makes an object
// of it. None is composed from a name, because none has one; a category over
// the unit alone is what lets the unit that defines one and whoever calls it
// arrive at the same symbol with nothing shared between them, and a unit
// publishes at most one of each.
auto NamespaceStorageInstallSymbol(std::string_view unit_name) -> std::string;
auto NamespaceStorageInitializeSymbol(std::string_view unit_name)
    -> std::string;
auto ObjectEntrySymbol(std::string_view unit_name) -> std::string;
// A unit's own storage. A package variable answers to the identifier the source
// declared, which is what another unit reaches it by; the cell a subroutine's
// static-lifetime local keeps answers to none, and takes its position instead.
auto NamespaceVariableSymbol(std::string_view unit_name, SymbolPart variable)
    -> std::string;

// The run-time description of one of a unit's types. The source declares no
// such thing, so the position the description sits at in its unit's pool is the
// whole of what identifies it.
auto TypeDescriptionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;
// The body building one of a unit's constants. The source wrote the value and
// never a name for it, so the position it sits at in its unit's pool is the
// whole of what identifies it.
auto IntegralConstantSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// A closure is counted rather than named, having no declaration of the source
// to take a name from, so its body's symbol is composed from its ordinal.
auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// The symbol `type`'s definition is linked under, or nothing where the type
// names no declaration a value is built from. The definition is the compiler's
// own and stands beside the declaration rather than inside it, so it is a
// category over the declaration's parts. A declaration of this unit and one
// another unit declares are composed from the same parts the same way, which is
// what lets every unit naming a class reach the constant its own unit emits
// with no shared table.
auto DefinitionSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

// The same for a declaration of this unit, which always has one.
auto DefinitionSymbol(const CompilationUnit& unit, ClassId id) -> std::string;
auto DefinitionSymbol(const CompilationUnit& unit, ClosureId id) -> std::string;

// The symbol of a declaration's constructor prologue, given the symbol its
// definition is linked under: what a constructor does once its base is built
// and before its body runs -- the value takes the declaration's tables, and its
// own members come into existence. It stands beside the definition, so it is
// composed over that symbol.
auto ConstructorPrologueSymbol(std::string_view definition) -> std::string;

// The destructors the Itanium C++ ABI gives a class with a virtual destructor:
// the base object destructor (D2) ends what the declaration declares and then
// what it extends; the complete object destructor (D1) ends a whole value; the
// deleting destructor (D0) also gives the value's storage back. A declaration
// extending one of another unit names its base's base object destructor this
// way.
enum class Destructor : std::uint8_t {
  kBaseObject,
  kCompleteObject,
  kDeleting
};

auto DestructorSymbol(std::string_view definition, Destructor which)
    -> std::string;

// The symbol of one constant a declaration holds, by the position it sits at
// among them.
auto ClassConstantSymbol(std::string_view definition, std::uint32_t ordinal)
    -> std::string;

// The symbols of the table a value of a class dispatches through and of the
// description of the class a cast reads, composed over the symbol its
// definition is linked under the same way. A class extending one of another
// unit names its base's description this way.
auto DispatchTableSymbol(std::string_view definition) -> std::string;
auto TypeInfoSymbol(std::string_view definition) -> std::string;

}  // namespace lyra::lir
