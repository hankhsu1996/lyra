#pragma once

// How a compilation unit is identified and what it is called. A unit's identity
// is its definition together with everything the parent fixed that changes what
// gets compiled; a name is derived from that identity where a bounded
// identifier is needed. The two are separate on purpose -- an identity has to
// distinguish and may drop nothing, a name has to fit in an identifier -- so
// nothing here treats one as the other.
//
// Both are computed from the frontend and from a policy every party reads the
// same, because the unit naming itself and every unit naming it must reach the
// same answer with no shared table.

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/climb.hpp"

namespace slang {
class ConstantValue;
}  // namespace slang

namespace slang::ast {
class ClassType;
class DefinitionSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Scope;
class Symbol;
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a value parameter's value comes from, for the objects built from one
// compiled scope. Fixed by the specialization: it is part of what is compiled,
// so every object of the scope holds the one value. Supplied at construction:
// whoever builds the object passes it -- an instance's overridden parameter
// (LRM 23.10.2), or the index a loop's block was built at (LRM 27.4). Computed
// at construction: its own declaration writes it from a supplied value,
// directly or through another such, so each object computes it when built.
enum class ParameterValueSource {
  kFixedBySpecialization,
  kSuppliedAtConstruction,
  kComputedAtConstruction,
};

// Which value parameters of each instance reach its unit when the instance is
// built rather than as part of what is compiled. A parameter's value is fixed
// before the run (LRM 23.10), and nothing requires the compiled unit to hold
// it: one whose value decides what is compiled -- a type, which blocks exist,
// which child is built -- makes two values two units; one only ever read as a
// value makes every value one unit, and the instance is handed it when it is
// built.
//
// A definition kept whole supplies nothing: an earlier lowering found two of
// its instances handed different values lowered apart. Every party naming a
// unit reads the same policy, so a parent and the child it names still agree.
//
// The answer is worked out from the source once per instance and kept, which
// is why the lookup is const and the table is not: naming asks it many times
// per instance. Working out one instance's answer asks for its children's,
// since whether what a child is handed is only read as a value is the child's
// answer.
class SpecializationPolicy {
 public:
  explicit SpecializationPolicy(
      std::unordered_set<const slang::ast::DefinitionSymbol*> kept_whole = {})
      : kept_whole_(std::move(kept_whole)) {
  }

  // The parameters `inst` is handed when it is built, in the order its body
  // declares them -- which is the order a construction supplies their values
  // in, for the parent writing the construction and the unit reading it alike.
  [[nodiscard]] auto SuppliedParametersOf(
      const slang::ast::InstanceSymbol& inst) const
      -> std::span<const slang::ast::ParameterSymbol* const>;

  // Where the value of `param`, declared anywhere in `inst`'s body, comes from
  // for the objects built from the compiled scope declaring it. A generate
  // block is built once per object of its scope, so every parameter it declares
  // differs between them: its index is supplied, and each other one computed.
  [[nodiscard]] auto ValueSourceOf(
      const slang::ast::InstanceSymbol& inst,
      const slang::ast::ParameterSymbol& param) const -> ParameterValueSource;

  // Where each hierarchical name `inst`'s body writes lands once it leaves the
  // instance, in the order the body writes them.
  [[nodiscard]] auto ClimbsOutOf(const slang::ast::InstanceSymbol& inst) const
      -> std::span<const ClimbAnchor>;

  // Naming one instance can ask for the name of another, where a name it writes
  // lands there, and that one may land back in the first: a path from the top
  // can name any instance (LRM 23.6), the one writing it included. These record
  // the chain of instances whose names are being worked out, so a name landing
  // on one of them is told by how far out on the chain it is rather than by a
  // name nobody has yet.
  void EnterNaming(const slang::ast::InstanceBodySymbol& body) const;
  void LeaveNaming() const;
  [[nodiscard]] auto LevelsOutTo(const slang::ast::InstanceBodySymbol& body)
      const -> std::optional<std::uint32_t>;

 private:
  struct PerInstance {
    std::vector<const slang::ast::ParameterSymbol*> supplied;
    std::unordered_set<const slang::ast::ParameterSymbol*> varying;
  };

  auto Of(const slang::ast::InstanceSymbol& inst) const -> const PerInstance&;
  auto Classify(const slang::ast::InstanceSymbol& inst) const -> PerInstance;

  std::unordered_set<const slang::ast::DefinitionSymbol*> kept_whole_;
  mutable std::unordered_map<const slang::ast::InstanceSymbol*, PerInstance>
      per_instance_;
  mutable std::unordered_map<
      const slang::ast::InstanceSymbol*, std::vector<ClimbAnchor>>
      climbs_;
  mutable std::vector<const slang::ast::InstanceBodySymbol*> naming_;
};

// The instantiation a body was elaborated for. A body is what one application
// of a definition produced and states no bindings apart from that application,
// so it belongs to exactly one and is never asked what it is a specialization
// of on its own.
[[nodiscard]] auto InstantiationOf(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol&;

// A value as an identity: every bit of it, unknowns included, so two values
// that differ anywhere never answer alike. The front end's own rendering is
// for reading -- it shortens a value wider than it cares to print and folds
// unknowns -- so two different values can render alike through it.
[[nodiscard]] auto ValueIdentity(const slang::ConstantValue& value)
    -> std::string;

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

// A parameter the parent overrode with a value the instance is handed when it
// is built. The value is not part of what is compiled, but that the parent
// supplies one is: an instance that is handed a value and one that works its
// default out from the declaration are built by different constructions.
struct SuppliedAtConstruction {
  auto operator==(const SuppliedAtConstruction&) const -> bool = default;
};

// The scope a hierarchical name lands in once it leaves the instance, named
// the way the class of that scope is. The name resolves per instance (LRM
// 23.8), and what it reaches from there are that scope's declarations, so two
// instances whose names land in different scopes compile differently.
struct LandsIn {
  std::string scope;

  auto operator==(const LandsIn&) const -> bool = default;
};

// The same, where the name lands in an instance whose own name is still being
// worked out -- the instance writing it, or one whose name asked for this
// one's. `levels` counts how far out on that chain it is, the instance itself
// being none, and `blocks` is the path of generate blocks below it the name
// lands in. Stated so, the scope is named by where it stands relative to the
// instance being named, which is all two instances could differ in there.
struct LandsBack {
  std::uint32_t levels = 0;
  std::vector<std::string> blocks;

  auto operator==(const LandsBack&) const -> bool = default;
};

using SpecializationInputKind = std::variant<
    FixedValue, FixedType, FixedInterface, SuppliedAtConstruction, LandsIn,
    LandsBack>;

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

// The key of the specialization `inst` is an application of, read off what its
// parent fixed for it: its parameters (LRM 6.20, 23.10), the interface each of
// its interface ports is connected to (LRM 25.3), and the scope each name its
// body writes lands in once it leaves the instance (LRM 23.8). A parameter
// fixed by the specialization enters with its value; one supplied at
// construction enters only as being supplied, and one computed at construction
// not at all, so instances supplied different values are one unit. Two
// instances compile alike exactly when every part agrees.
//
// Every part is read at the instantiation, which is where a parent fixed it and
// the only place all of it is stated for one instance. What the frontend chose
// to elaborate once serves a question about its own work and settles nothing
// here.
auto SpecializationKeyOf(
    const slang::ast::InstanceSymbol& inst, const SpecializationPolicy& policy)
    -> SpecializationKey;

// The key of a SystemVerilog class specialization (LRM 8.25). Two
// specializations of one generic class denote the same type iff every value
// binding is equal and every type binding is a matching type (LRM 8.25
// uniqueness rule); slang deduplicates on that rule, so distinct bindings
// arrive as distinct ClassType instances and key apart. Bare `C` and
// empty-override `C #()` resolve to the same slang ClassType and key alike.
auto SpecializationKeyOf(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> SpecializationKey;

// The name a key is known by. The definition's name when nothing was fixed, and
// otherwise that name plus a content hash of the key -- bounded, so it serves
// as an identifier, and computed by folding only bytes, so the producer (the
// unit naming itself) and every consumer (a parent naming a child) reach the
// same answer across separate compilations with no shared table.
auto SpecializationName(const SpecializationKey& key) -> std::string;

// The name the specialization `inst` is an application of, for a caller that
// wants the name and not the key it comes from.
auto SpecializationName(
    const slang::ast::InstanceSymbol& inst, const SpecializationPolicy& policy)
    -> std::string;

// The name the class specialization `cls` is, for a caller that wants the name
// and not the key it comes from.
auto SpecializationName(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> std::string;

// The name of the class an object standing for `scope` is: the unit's, for an
// instance's body, and the unit's followed by the generate blocks down to it,
// each as the hierarchy spells it (LRM 27.6), for a generate block.
auto ScopeClassName(
    const slang::ast::Scope& scope, const SpecializationPolicy& policy)
    -> std::string;

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

// The structural scope whose instance `cls` is a type of (LRM 6.22): the
// nearest instance body or generate block enclosing it, or the package or
// `$unit` scope declaring it, which no instance replicates. A class nested
// inside another class is a type of the same instance the outer one is, since
// SystemVerilog gives it no reach into the outer object.
[[nodiscard]] auto DeclaringStructuralScope(const slang::ast::ClassType& cls)
    -> const slang::ast::Scope&;

// Whether `unit` is a design element (LRM 23.2.1) rather than a namespace one.
// A design element is instantiated into the hierarchy, so a type it declares
// inside is a type of each instance rather than one type of the unit (LRM
// 6.22), and what it publishes is the object each instance is. A package and
// the file-set scope are named once and declare once.
[[nodiscard]] auto IsDesignElement(const slang::ast::Symbol& unit) -> bool;

// Whether `cls` belongs to an instance (LRM 6.22): a design element declares
// it, so each instance of the scope declaring it has a type of its own. A class
// a package or the `$unit` scope declares belongs to none, whichever unit
// reaches it.
[[nodiscard]] auto BelongsToAnInstance(const slang::ast::ClassType& cls)
    -> bool;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its declarations and the unit emitting that declaration agree with no
// shared table (LRM 26.3). A package publishes its declared name; a module body
// its specialization name. An anonymous compilation-unit scope (the LRM 3.12.1
// `$unit` file-set scope, modeled as a namespace unit with no source name)
// publishes a name derived from its own source-input identity: the only
// property distinguishing two such scopes is which compilation-unit input they
// belong to, which the LRM uses to define the scope boundary itself. Both the
// producer and every consumer compute this from the same slang unit symbol.
auto CompilationUnitName(
    const slang::ast::Symbol& unit, const SpecializationPolicy& policy)
    -> std::string;

// The name a type declaration has inside the unit declaring it, for a type
// SystemVerilog identifies by its declaration (LRM 6.22.1): the scopes between
// the unit and the declaration, then what the declaration answers to there.
// Together with the declaring unit's name it identifies the type from anywhere,
// and the unit declaring it and every unit naming it compute it alike from the
// same frontend symbol.
auto TypeDeclarationName(
    const slang::ast::Type& type, const SpecializationPolicy& policy)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
