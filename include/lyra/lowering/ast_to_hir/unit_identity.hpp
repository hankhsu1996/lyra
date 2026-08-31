#pragma once

// How a compilation unit is identified and what it is called. A unit's identity
// is its definition together with everything the parent fixed that changes what
// gets compiled; a name is derived from that identity where a bounded
// identifier is needed. The two are separate on purpose -- an identity has to
// distinguish and may drop nothing, a name has to fit in an identifier -- so
// nothing here treats one as the other.
//
// Both are computed from the frontend alone, because the unit naming itself and
// every unit naming it must reach the same answer with no shared table.

#include <string>
#include <variant>
#include <vector>

namespace slang::ast {
class ClassType;
class InstanceBodySymbol;
class InstanceSymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What one input to a specialization was fixed to. A value and a type are the
// two a parameter takes (LRM 6.20.2, 6.20.3); an interface is what an interface
// port carries (LRM 25.3), which no parameter declares and the language gives
// no way to write, so the connection is where it is fixed. Each holds the
// identity of what it was fixed to -- a constant's value, a data type's
// identity, a unit's name -- because that is what decides whether two
// instantiations compile alike.
struct FixedValue {
  std::string value;

  auto operator==(const FixedValue&) const -> bool = default;
};

struct FixedType {
  std::string type;

  auto operator==(const FixedType&) const -> bool = default;
};

struct FixedInterface {
  std::string unit_name;
  // The modport the port is restricted to (LRM 25.5), which narrows the members
  // it reaches and their directions. Empty when the port reaches the whole
  // interface, which the LRM spells as the absence of one.
  std::string modport;

  auto operator==(const FixedInterface&) const -> bool = default;
};

using SpecializationInputKind =
    std::variant<FixedValue, FixedType, FixedInterface>;

// One thing a parent fixed at an instantiation site: what it named, and what it
// fixed that to. Every input is named -- a parameter by its own name, an
// interface port by the port's -- so the name sits here and the arms carry only
// what differs between them.
struct SpecializationInput {
  std::string name;
  SpecializationInputKind kind;

  auto operator==(const SpecializationInput&) const -> bool = default;
};

// Which compiled artifact an instance belongs to: the definition it is built
// from, and everything the parent fixed that changes what gets compiled. Two
// instances with equal keys compile alike and share one artifact; instance
// count never affects how many keys exist. Equality is structural, so two keys
// agree or differ on their parts and never on a rendering of them.
struct SpecializationKey {
  std::string definition;
  std::vector<SpecializationInput> inputs;

  auto operator==(const SpecializationKey&) const -> bool = default;
};

// The key of the specialization `body` is, read off the bindings that made it
// one: its resolved parameters (LRM 6.20, 23.10) and the interface each of its
// interface ports carries (LRM 25.3). Instances agreeing on all of it share
// slang's one canonical body and so key alike; instances differing anywhere key
// apart, because they compile against different types at different positions.
auto SpecializationKeyOf(const slang::ast::InstanceBodySymbol& body)
    -> SpecializationKey;

// The key of a SystemVerilog class specialization (LRM 8.25). Two
// specializations of one generic class denote the same type iff every value
// binding is equal and every type binding is a matching type (LRM 8.25
// uniqueness rule); slang deduplicates on that rule, so distinct bindings
// arrive as distinct ClassType instances and key apart. Bare `C` and
// empty-override `C #()` resolve to the same slang ClassType and key alike.
auto SpecializationKeyOf(const slang::ast::ClassType& cls) -> SpecializationKey;

// The name a key is known by. The definition's name when nothing was fixed, and
// otherwise that name plus a content hash of the key -- bounded, so it serves
// as an identifier, and computed by folding only bytes, so the producer (the
// unit naming itself) and every consumer (a parent naming a child) reach the
// same answer across separate compilations with no shared table.
auto SpecializationName(const SpecializationKey& key) -> std::string;

// The name the specialization `body` is, for a caller that wants the name and
// not the key it comes from.
auto SpecializationName(const slang::ast::InstanceBodySymbol& body)
    -> std::string;

// Resolves the instance to its canonical body and names that specialization.
auto SpecializationName(const slang::ast::InstanceSymbol& inst) -> std::string;

// The name the class specialization `cls` is, for a caller that wants the name
// and not the key it comes from.
auto SpecializationName(const slang::ast::ClassType& cls) -> std::string;

// The symbol whose compilation unit owns `decl`, found by climbing its parent
// scopes to the first that is one: a package (LRM 26), a design element's body
// (LRM 23.2, 25), or the `$unit` file-set scope a declaration outside every
// design element belongs to (LRM 3.12.1). A declaration nested deeper -- in a
// class, a generate block, a subroutine -- still hits one of those first, which
// is the ownership boundary. Every declaration lies in some compilation unit,
// so one is always found, and a chain that ends without one is a declaration
// the frontend placed nowhere.
auto DeclaringCompilationUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol&;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its members by name and the unit emitting that member agree with no shared
// table (LRM 26.3). A package publishes its declared name; a module body its
// specialization name. An anonymous compilation-unit scope (the LRM 3.12.1
// `$unit` file-set scope, modeled as a namespace unit with no source name)
// publishes a name derived from its own source-input identity: the only
// property distinguishing two such scopes is which compilation-unit input they
// belong to, which the LRM uses to define the scope boundary itself. Both the
// producer and every consumer compute this from the same slang unit symbol.
auto CompilationUnitName(const slang::ast::Symbol& unit) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
