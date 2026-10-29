#pragma once

// How an application of a definition is identified and what it is called. A
// specialization's identity is its definition together with everything the
// design fixed for it; a name is derived from that identity where a bounded
// identifier is needed. The two are separate on purpose -- an identity has to
// distinguish and may drop nothing, a name has to fit in an identifier -- so
// nothing here treats one as the other.
//
// Both are computed from the frontend and from a naming every party in one
// stage reads the same, because the unit naming itself and every unit naming
// it must reach the same answer with no shared table.

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/instance_context.hpp"
#include "lyra/support/bodies_read.hpp"
#include "lyra/support/def_path.hpp"

namespace slang {
class ConstantValue;
}  // namespace slang

namespace slang::ast {
class ClassType;
class CovergroupType;
class DefinitionSymbol;
class GenerateBlockSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Scope;
class Symbol;
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a value parameter's value comes from, for the objects built from one
// compiled scope. Settled at elaboration: the code reads it as the constant
// this specialization settled it to. Supplied at construction: whoever builds
// the object passes it -- an instance's parameter its code reads (LRM 23.10.2),
// or the index a loop's block was built at (LRM 27.4). Computed at
// construction: its own declaration writes it from a supplied value, directly
// or through another such, so each object computes it when built.
enum class ParameterValueSource {
  kSettledAtElaboration,
  kSuppliedAtConstruction,
  kComputedAtConstruction,
};

// The instantiation a body was elaborated for. A body is what one application
// of a definition produced and states no bindings apart from that application,
// so it belongs to exactly one and is never asked what it is a specialization
// of on its own.
[[nodiscard]] auto InstantiationOf(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol&;

// The instance `scope` is the body of or stands in: an instance's body, or a
// scope declared somewhere inside one.
[[nodiscard]] auto InstanceHolding(const slang::ast::Scope& scope)
    -> const slang::ast::InstanceSymbol&;

// Whether `scope`, or a generate block it builds, declares a class or a
// covergroup (LRM 8.3, 19.2, 27.3), each a type of the scope declaring it
// alone. A child instance's body is the child's, so it is not reached.
[[nodiscard]] auto DeclaresAClass(const slang::ast::Scope& scope) -> bool;

// The digest a name carries of `bytes`: the first 64 bits of their stable
// fingerprint, so producer and consumer agree on the name across separately
// compiled units and across sessions. Whoever names by it holds the name
// against what it was folded from, which is where two folding to one digest is
// caught.
[[nodiscard]] auto NameDigest(std::string_view bytes) -> std::uint64_t;

// A declaration's path in its unit as an identity: each step as its kind and
// then the length of its name and the name, or the position it stands at and a
// terminator, and after a name whatever else tells the step from its siblings,
// each behind a mark no kind is written with. So two paths that differ anywhere
// never answer alike whatever characters a name holds (LRM 5.6.1).
[[nodiscard]] auto DefPathIdentity(const support::DefPath& path) -> std::string;

// A value as an identity: every bit of it, unknowns included, so two values
// that differ anywhere never answer alike. The front end's own rendering is
// for reading -- it shortens a value wider than it cares to print and folds
// unknowns -- so two different values can render alike through it.
[[nodiscard]] auto ValueIdentity(const slang::ConstantValue& value)
    -> std::string;

// What one input to a specialization's key was fixed to. A value and a type
// are the two a parameter takes (LRM 6.20.2, 6.20.3); an interface is what an
// interface port carries (LRM 25.3), which no parameter declares and the
// language gives no way to write, so the connection is where it is fixed. Each
// holds the identity of what it was fixed to -- a constant's value, a data
// type's identity, a specialization's name -- because that is what decides
// whether two instantiations elaborate alike.
struct FixedValue {
  std::string value;

  auto operator==(const FixedValue&) const -> bool = default;
};

struct FixedType {
  std::string type;

  auto operator==(const FixedType&) const -> bool = default;
};

// The interface instances an interface port is bound to, as the
// specializations they are: the distinct ones, in the order the positions
// first meet them, and which of them each position takes, in row-major order
// (LRM 25.3). A port standing for one instance is the set of one.
struct FixedInterface {
  std::vector<std::string> specializations;
  std::vector<std::uint32_t> taken;
  // The modport the port is restricted to (LRM 25.5), which narrows the members
  // it reaches and their directions. Empty when the port reaches the whole
  // interface, which the LRM spells as the absence of one.
  std::string modport;

  auto operator==(const FixedInterface&) const -> bool = default;
};

// The scope a hierarchical name lands in once it leaves the instance, named by
// the specialization holding it and the scope's path there. The name resolves
// per instance (LRM 23.8), and what it reaches from there are that scope's
// declarations, so two instances whose names land in different scopes
// elaborate differently. `reads` is the constant the name reads there, where
// it reads one.
struct LandsIn {
  std::string specialization;
  support::DefPath scope;
  std::optional<std::string> reads;

  auto operator==(const LandsIn&) const -> bool = default;
};

// The same, where the name lands in an instance whose own key is still being
// worked out -- the instance writing it, or one whose key asked for this one's.
// `levels` counts how far out on that chain it is, the instance itself being
// none, and `blocks` names the generate blocks below it the name lands in.
// Stated so, the scope is named by where it stands relative to the instance
// being named, which is all two instances could differ in there.
struct LandsBack {
  std::uint32_t levels = 0;
  support::DefPath blocks;
  std::optional<std::string> reads;

  auto operator==(const LandsBack&) const -> bool = default;
};

// An instantiation a bind directive inserted into the instance (LRM 23.11),
// named by the directive. The bound instantiation's connections are text of
// the directive, read from the target's point of view, so two targets bound by
// one directive build alike and two directives may connect one name apart.
struct BindInstantiation {
  std::string directive;

  auto operator==(const BindInstantiation&) const -> bool = default;
};

// The cell a configuration bound the instance to (LRM 33.4), as its library
// and its name. Under a configuration the same instantiation is bound to
// different cells depending on where its instance stands, so the text holding
// it does not say which.
struct BoundToCell {
  std::string cell;

  auto operator==(const BoundToCell&) const -> bool = default;
};

using UnitKeyInputKind = std::variant<
    FixedValue, FixedType, FixedInterface, LandsIn, LandsBack,
    BindInstantiation, BoundToCell>;

// One thing the design fixed for an instance: what it named, and what it fixed
// that to. Every input is named -- a parameter by its own name, an interface
// port by the port's, what reaches an instance below by that instance's path
// from this one -- so the name sits here and the arms carry only what differs
// between them.
struct UnitKeyInput {
  std::string name;
  UnitKeyInputKind kind;

  auto operator==(const UnitKeyInput&) const -> bool = default;
};

// What an application of a definition is: the definition, and everything the
// design fixed for it. Two instances with equal keys elaborate alike and lower
// alike, whatever their code turns out to share with another. Equality is
// structural, so two keys agree or differ on their parts and never on a
// rendering of them.
struct UnitKey {
  std::string definition;
  std::vector<UnitKeyInput> inputs;

  auto operator==(const UnitKey&) const -> bool = default;
};

class Specializations;
class UnitNamingPolicy;

// The keys worked out in one naming, each kept for its instance, and each name
// kept held against the key it was folded from: two keys reaching one name
// would silently make two into one.
//
// Working out one key can ask for another's -- where a name it writes lands
// (LRM 23.6), where a type it is fixed to is declared (LRM 6.22), where its own
// definition is nested (LRM 23.4) -- and the answer may be an instance whose
// key in this naming is still being worked out, the one asking included. So the
// table holds the chain of instances being worked out in its naming, and such
// an instance is told by how far out on the chain it is rather than by a key
// nobody has yet. A key asked in another naming meanwhile is worked out on that
// naming's chain and is no part of this one.
class KeyTable {
 public:
  explicit KeyTable(const UnitNamingPolicy& naming) : naming_(&naming) {
  }

  // The name `inst` goes by in this table's naming: its key, every other
  // instance and every block in it named as that naming names them, worked out
  // and kept where it can be.
  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst)
      -> std::string;

  // The key the name `name` was folded from.
  [[nodiscard]] auto KeyNamed(const std::string& name) const -> const UnitKey&;

  // Drops what was kept of `inst`, so it is worked out again when next asked.
  void Forget(const slang::ast::InstanceSymbol& inst);

  // How far out on this table's chain `body` stands, or nothing where it is not
  // on it.
  [[nodiscard]] auto LevelsOutTo(const slang::ast::InstanceBodySymbol& body)
      -> std::optional<std::uint32_t>;

 private:
  struct KeptKey {
    UnitKey key;
    std::string name;
    // The instances it looked for among those being worked out and did not
    // find.
    std::unordered_set<const slang::ast::InstanceBodySymbol*> depends_on;
  };

  struct KeyInProgress {
    // How long the chain was when this key was asked for, so a landing on an
    // earlier entry is one outside it.
    std::size_t chain_at_start = 0;
    bool met_nothing_outside = true;
    std::unordered_set<const slang::ast::InstanceBodySymbol*> depends_on;
  };

  // Whether `kept` still answers: none of the instances it looked for and did
  // not find is being worked out.
  [[nodiscard]] auto AnswersNow(const KeptKey& kept) const -> bool;

  const UnitNamingPolicy* naming_;
  std::unordered_map<const slang::ast::InstanceSymbol*, KeptKey> kept_;
  std::unordered_map<std::string, UnitKey> folded_;
  std::vector<const slang::ast::InstanceBodySymbol*> chain_;
  std::vector<KeyInProgress> in_progress_;
};

// How one stage of the compile names what it names: the instance a reference
// reaches, and a generate block on the way to a declaration. Declaring a unit
// and lowering its bodies state another instance's class and a block's class
// in the stage's own terms -- the specialization it is while keys are worked
// out, what it reaches while specializations are compared, the class grouping
// found for it once they have been, and what it is built as while that is
// worked out -- and every other answer is the specializations' own, the same
// in every stage.
//
// Every party in one stage reads the same naming, so a parent and the child it
// names agree.
//
// A naming keys are worked out in is made with the table they are kept in,
// whose chain says which of them are still being worked out; a naming no key
// is worked out in has none.
class UnitNamingPolicy {
 public:
  UnitNamingPolicy(
      const Specializations& specializations, KeyTable* keys_worked_out)
      : specializations_(&specializations), keys_worked_out_(keys_worked_out) {
  }
  UnitNamingPolicy(const UnitNamingPolicy&) = delete;
  auto operator=(const UnitNamingPolicy&) -> UnitNamingPolicy& = delete;
  UnitNamingPolicy(UnitNamingPolicy&&) = delete;
  auto operator=(UnitNamingPolicy&&) -> UnitNamingPolicy& = delete;
  virtual ~UnitNamingPolicy() = default;

  // What `inst` is named by in this stage.
  [[nodiscard]] virtual auto NameOf(
      const slang::ast::InstanceSymbol& inst) const -> std::string = 0;

  // The step the block instance `block` is in the scope holding it: its label,
  // which one it is among the blocks of that scope sharing the label (LRM
  // 27.5), and what tells it from the other block instances of its construct in
  // this stage.
  [[nodiscard]] virtual auto BlockStepOf(
      const slang::ast::GenerateBlockSymbol& block) const
      -> support::GenerateBlockStep = 0;

  // How far out on the chain of keys being worked out in this naming `body`
  // stands, or nothing where it is not on it.
  [[nodiscard]] auto LevelsOutTo(const slang::ast::InstanceBodySymbol& body)
      const -> std::optional<std::uint32_t> {
    if (keys_worked_out_ == nullptr) return std::nullopt;
    return keys_worked_out_->LevelsOutTo(body);
  }

  // The specializations every stage names the instances of.
  [[nodiscard]] auto Keys() const -> const Specializations& {
    return *specializations_;
  }

 private:
  const Specializations* specializations_;
  KeyTable* keys_worked_out_;
};

// The naming of the stage that works the keys out: another instance by the
// specialization it is, and a block by where it stands -- its label, and in a
// loop the value its index stood at (LRM 27.4).
class SpecializationNames final : public UnitNamingPolicy {
 public:
  using UnitNamingPolicy::UnitNamingPolicy;

  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;
  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override;
};

// The specializations of the design, each by its key, and what follows of each
// instance from its body alone. A key holds every value the instance is handed
// and every fact written elsewhere that reaches it: which specializations
// publish alike, and which come out as the same code, is found by comparing
// what they publish and what they lower to.
//
// Each answer is worked out from the source once and kept, which is why the
// lookups are const and the tables are not.
class Specializations {
 public:
  explicit Specializations(
      support::BodiesRead bodies_read = support::BodiesRead::kOnePerName)
      : bodies_read_(bodies_read), names_(*this, &keys_), keys_(names_) {
  }
  Specializations(const Specializations&) = delete;
  auto operator=(const Specializations&) -> Specializations& = delete;
  Specializations(Specializations&&) = delete;
  auto operator=(Specializations&&) -> Specializations& = delete;
  ~Specializations() = default;

  // The naming the keys are worked out in.
  [[nodiscard]] auto Names() const -> const UnitNamingPolicy& {
    return names_;
  }

  // The name of the specialization `inst` is an application of: its
  // definition, with the digest of everything the design fixed for it, where it
  // fixed anything -- each parameter's value or type (LRM 6.20, 23.10), the
  // interface each interface port is connected to (LRM 25.3), the scope each
  // name written in it or below it lands in once it leaves the instance (LRM
  // 23.8), and everything written elsewhere that reaches an instance below it
  // -- a parameter a defparam or a configuration sets (LRM 23.10.1, 33.4.3), an
  // instantiation a bind inserts (LRM 23.11), a cell a configuration binds (LRM
  // 33.4.1.6) -- each under its path from `inst` -- and the constant each such
  // name reads where it lands. Instances of one specialization are built
  // alike.
  //
  // Every part is read off `inst` and what elaborated below it, which is where
  // a parent naming a child already stands. An instance the front end left
  // sharing another's body is read off that body, which states the same for
  // both.
  [[nodiscard]] auto SpecializationNameOf(
      const slang::ast::InstanceSymbol& inst) const -> std::string;

  // Whether the design fixed nothing for `inst`, so that its definition alone
  // says what it is.
  [[nodiscard]] auto FixesNothing(const slang::ast::InstanceSymbol& inst) const
      -> bool;

  // The definition `inst` is an application of, as its key names it.
  [[nodiscard]] auto DefinitionOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string;

  // Whether this compile reads every instance's body and holds each to the
  // body of the instance it repeats, which is how a compile is checked against
  // itself.
  [[nodiscard]] auto ChecksItself() const -> bool;

  // Whether `inst` is read through a body of its own: one the front end
  // elaborated for it, or one this compile decided to read.
  [[nodiscard]] auto HasBodyOfItsOwn(
      const slang::ast::InstanceSymbol& inst) const -> bool;

  // Has `inst` read through its own body from here on, which makes the front
  // end elaborate it. Whatever was worked out of it through the shared body is
  // dropped and worked out again from its own, and its specialization must come
  // out the same.
  void ReadBodyOf(const slang::ast::InstanceSymbol& inst) const;

  // The parameters `inst` is handed when it is built, in the order its body
  // declares them -- which is the order a construction supplies their values
  // in, for the parent writing the construction and the unit reading it alike.
  [[nodiscard]] auto SuppliedParametersOf(
      const slang::ast::InstanceSymbol& inst) const
      -> std::span<const slang::ast::ParameterSymbol* const>;

  // Where the value of `param`, declared anywhere in `inst`'s body, comes from
  // for the objects built from the compiled scope declaring it.
  [[nodiscard]] auto ValueSourceOf(
      const slang::ast::InstanceSymbol& inst,
      const slang::ast::ParameterSymbol& param) const -> ParameterValueSource;

  // What follows for `inst` from where it stands, worked out once and kept.
  [[nodiscard]] auto ContextOf(const slang::ast::InstanceSymbol& inst) const
      -> const InstanceContext&;

 private:
  // Where each value parameter of an instance's body takes its value from, and
  // those its objects are handed, in the order the body declares them.
  struct ParametersOfInstance {
    std::vector<const slang::ast::ParameterSymbol*> supplied;
    std::unordered_map<const slang::ast::ParameterSymbol*, ParameterValueSource>
        sources;
  };

  [[nodiscard]] auto ParametersOf(const slang::ast::InstanceSymbol& inst) const
      -> const ParametersOfInstance&;
  [[nodiscard]] auto ReadThroughItsOwnBody() const -> HasBodyOfItsOwnFn;

  support::BodiesRead bodies_read_;
  SpecializationNames names_;
  mutable std::unordered_set<const slang::ast::InstanceSymbol*> read_anyway_;
  mutable std::unordered_map<
      const slang::ast::InstanceSymbol*, ParametersOfInstance>
      per_instance_;
  mutable InstanceContexts context_;
  mutable KeyTable keys_;
};

// The key of a SystemVerilog class specialization (LRM 8.25). Two
// specializations of one generic class denote the same type iff every value
// binding is equal and every type binding is a matching type (LRM 8.25
// uniqueness rule); slang deduplicates on that rule, so distinct bindings
// arrive as distinct ClassType instances and key apart. Bare `C` and
// empty-override `C #()` resolve to the same slang ClassType and key alike.
auto UnitKeyOf(const slang::ast::ClassType& cls, const UnitNamingPolicy& policy)
    -> UnitKey;

// The digest of everything a key says was fixed, or nothing for a key under
// which nothing was. It is computed by folding only bytes, so the producer (the
// unit naming itself) and every consumer (a parent naming a child) reach the
// same answer across separate compilations with no shared table.
auto ArgumentsDigest(const UnitKey& key) -> std::optional<std::uint64_t>;

// The name a key is known by. The definition's name when nothing was fixed, and
// otherwise that name plus the digest of what was -- bounded, so it serves as
// an identifier.
auto UnitName(const UnitKey& key) -> std::string;

// The name the class specialization `cls` is, for a caller that wants the name
// and not the key it comes from.
auto UnitName(const slang::ast::ClassType& cls, const UnitNamingPolicy& policy)
    -> std::string;

// The step a subroutine or a block of statements is in the scope holding it:
// under its name (LRM 13, 9.3.4), or for a block the source gave no label by
// where it sits. The unit publishing what such a scope holds and a unit
// reaching into it by a hierarchical name (LRM 23.9) both ask here.
auto ProceduralStepOf(const slang::ast::Symbol& scope) -> support::DefPathData;

// Which declaration of its compilation unit `declaration` is: one step for each
// scope the source nests it in, from the unit's own scope down, and one for the
// declaration itself, which is a scope too -- a generate block as the
// application of its definition it is (LRM 27.3), a class under its name and
// what its parameters were bound to (LRM 8.3, 8.25), a subroutine or a labelled
// block under its name (LRM 13, 9.3.4), an enumeration, a structure or a union
// under the name it answers to (LRM 6.22.1), and a block or type with no name
// by where it sits. The unit's own scope is the path of no steps, so the block
// instances of one application are one declaration, as the instances of one
// unit are.
//
// The source alone decides the answer, so the unit declaring something and
// every unit naming it compute the same path here, and a type SystemVerilog
// identifies by its declaration (LRM 8.3, 6.22.1) is identified from anywhere
// by the name of the unit holding it and this path.
auto DefPathOf(
    const slang::ast::Symbol& declaration, const UnitNamingPolicy& policy)
    -> support::DefPath;

// The name a covergroup goes by (LRM 19.3): the one it was declared with, or
// for one a class embeds, which declares an anonymous type and a property of it
// (LRM 19.4), the property's.
auto CovergroupNameOf(const slang::ast::CovergroupType& covergroup)
    -> std::string_view;

// The package (LRM 26.2) or `$unit` scope (LRM 3.12.1) `decl` is declared in,
// or null where it is declared in any other scope. Neither has an instance, so
// what one declares is reached by the unit's name rather than routed to through
// an object. An anonymous program declares its items in the package or `$unit`
// scope around it without opening a scope of its own (LRM 24.6), so one of its
// items is declared in that unit.
[[nodiscard]] auto DeclaringNamespaceUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol*;

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

// The structural scope whose instances each have their own copy of `decl`, a
// declaration or a type (LRM 6.22): a type declared inside an instance is a
// type of that instance. A specialization of a generic class is a type of its
// own for each set of parameters (LRM 8.25), so one specialized on a type of an
// instance is a type of that instance too, wherever the generic was declared.
// So the answer is the innermost instance body or generate block among the one
// enclosing `decl` and those replicating each type argument of a specialization
// `decl` is or lies in; where none is, the package or `$unit` scope enclosing
// `decl`, which no instance replicates. A class nested inside another class is
// a type of the same instance the outer one is, since SystemVerilog gives it no
// reach into the outer object.
[[nodiscard]] auto ReplicatingScope(const slang::ast::Symbol& decl)
    -> const slang::ast::Scope&;

// Whether `scope` is a scope of the design hierarchy: a design element's body
// or a generate block, each of which every instance has its own of.
[[nodiscard]] auto IsInstanceScope(const slang::ast::Scope& scope) -> bool;

// The symbol whose compilation unit holds `decl`: the one whose own source
// fixes everything `decl` is, so no unit's content follows from how another
// unit uses its declarations. A declaration an instance replicates belongs to
// the design element's body that scope lies in. Otherwise, one that is or lies
// in a specialization of a generic class belongs to that specialization, which
// is a unit of its own, since its parameters are written wherever it is named
// and the unit declaring the generic fixes none of them (LRM 8.25). Every other
// declaration belongs to its declaring compilation unit.
[[nodiscard]] auto UnitHomeOf(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol&;

// Whether `unit` is a design element (LRM 23.2.1) rather than a namespace one.
// A design element is instantiated into the hierarchy, so a type it declares
// inside is a type of each instance rather than one type of the unit (LRM
// 6.22), and what it publishes is the object each instance is. A package and
// the file-set scope are named once and declare once.
[[nodiscard]] auto IsDesignElement(const slang::ast::Symbol& unit) -> bool;

// Whether `type`, a class or a covergroup, belongs to an instance (LRM 6.22):
// an instance replicates it, so each instance of that scope has a type of its
// own. One a package or the `$unit` scope declares belongs to none, whichever
// unit reaches it, unless it is a specialization on a type that does.
[[nodiscard]] auto BelongsToAnInstance(const slang::ast::Symbol& type) -> bool;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its declarations and the unit emitting that declaration agree with no
// shared table (LRM 26.3). A package publishes its declared name; a module body
// its unit name. An anonymous compilation-unit scope (the LRM 3.12.1 `$unit`
// file-set scope, modeled as a namespace unit with no source name) publishes a
// name derived from its own source-input identity: the only property
// distinguishing two such scopes is which compilation-unit input they belong
// to, which the LRM uses to define the scope boundary itself. A specialization
// that is a unit of its own publishes the name of the unit declaring its
// generic joined to its own the way a unit's name joins its definition to the
// rest, so the name is an identifier as a module's is. Both the producer and
// every consumer compute this from the same slang unit symbol.
auto CompilationUnitName(
    const slang::ast::Symbol& unit, const UnitNamingPolicy& policy)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
