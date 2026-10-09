#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/scope_info.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// A node in the one canonical object tree. Every constructed
// SystemVerilog scope -- a module instance, a generate block, the
// implicit `$root` -- is a Scope.
//
// It is a value of a class like any other, which is where what it is of comes
// from; what this kind adds is its place in the tree (parent plus
// HierarchySegment, its child scopes) and the phases the runtime drives it
// through. The members its class declares are the class's own, placed after
// this part by whatever laid the class out. The runtime walks this same tree;
// there is no parallel topology. Every dynamic scheduler concern -- queues,
// process registry, deferred-effect attribution, ambient identity -- lives on
// the Runtime, so beyond being a value of its class a scope contributes
// structural identity and nothing else.
class Scope : public GcObject {
 public:
  using ChildVisitor = std::function<void(Scope&)>;

  // `definition` has to be of a scope class.
  Scope(
      Scope* parent, HierarchySegment segment,
      const ObjectDefinition* definition);
  ~Scope() override;
  Scope(const Scope&) = delete;
  auto operator=(const Scope&) -> Scope& = delete;
  Scope(Scope&&) = delete;
  auto operator=(Scope&&) -> Scope& = delete;

  [[nodiscard]] auto Parent() const -> Scope*;

  // The scope's structural identity at this level: the parent-side label
  // plus per-dimension indices (empty for a scalar, `{i}` for a generate-for
  // iteration, `{i, j}` for a multi-dim instance array). Fixed at
  // construction and never observed before then.
  [[nodiscard]] auto Segment() const -> const HierarchySegment&;

  // The LRM 27.6 display form of `Segment()` -- `loop[0]`, `m`, `gblk`.
  [[nodiscard]] auto DisplaySegment() const -> std::string;

  // Joins each ancestor's display segment with `.`, ordered from the outermost
  // inward (LRM 21.2.1.5; `%m` resolves to this string). A scope the source
  // never named contributes nothing, which is what keeps it off every
  // hierarchical path (LRM 23.6) without taking it out of the tree. The walk
  // stops at the implicit `$root` so multi-top output reads `Top.mid.x` rather
  // than `$root.Top.mid.x`. Walked on demand: the object tree is sealed by
  // Activate.
  [[nodiscard]] auto HierarchicalPath() const -> lyra::value::String;

  // What this scope's class states of its instances beyond its type
  // information: its timescale, and the DPI-C exports an instance answers.
  [[nodiscard]] auto Info() const -> const ScopeInfo& {
    return *definition_->scope;
  }

  // The class this scope is a value of.
  [[nodiscard]] auto Definition() const -> const ObjectDefinition* {
    return definition_;
  }

  // Wires `child` into this scope's physical containment edge: sets
  // `child.parent_` to `this` and places `child` in the attached-children
  // relation (used by elaboration walks, dump, and ForEachChild). Called after
  // the typed owner commits the child, so a thrown ctor leaves no half-attached
  // scope.
  auto AddOwnedChild(std::unique_ptr<Scope> child) -> Scope*;

  // Whether this scope carries a source-visible SV name. An unnamed
  // begin/end (synthetic anonymous scope) is emitted with an empty
  // `HierarchySegment` base name; every other scope kind gets its SV
  // identifier as the base name. A hierarchical path leaves the unnamed ones
  // out (LRM 23.6).
  [[nodiscard]] auto IsAddressable() const -> bool {
    return !segment_.BaseName().empty();
  }

  // Where a hierarchical name leaving this instance starts: the nearest scope
  // above this one whose class is `cls` or extends it, or past the topmost of
  // them a top-level instance of it (LRM 23.6, 23.8). The front end resolved
  // the name to a declaration of that class, so one stands there in every
  // instance that resolution covered.
  [[nodiscard]] auto EnclosingScope(const ObjectDefinition* cls) -> Scope*;

  // Whether this scope is an object of the class `cls` or of one extending it.
  [[nodiscard]] auto IsOfClass(const ObjectDefinition* cls) const -> bool;

  // This instance's own source of seeds for the static processes and static
  // initializers declared within it (LRM 18.14.1). Meaningful on a module,
  // interface, or program instance; a caller names that instance rather than
  // asking a scope inside one to find it. Every instance's runs from the same
  // default seed, which is what keeps one instance's draws out of another's.
  [[nodiscard]] auto InitializationSeeds() -> InitializationRng&;

  // The scope's declared time precision as a power of ten (LRM Table 20-2),
  // read from its metadata; a scope with no timescale of its own reports the
  // unspecified sentinel. The engine takes the minimum across the tree to fix
  // the design-global precision (LRM 3.14.3).
  [[nodiscard]] auto TimePrecisionPower() const -> std::int8_t {
    return Info().metadata.time_precision_power;
  }

  // The scope's time unit as a power of ten (LRM Table 20-2), read from its
  // metadata: an addressable scope reports its own or inherited timescale, a
  // scope with none of its own the unspecified sentinel. Read by the DPI
  // `svGetTimeUnit` query and to scale `svGetTime` to the scope (LRM 35.5.3,
  // Annex H).
  [[nodiscard]] auto TimeUnitPower() const -> std::int8_t {
    return Info().metadata.time_unit_power;
  }

  // Per-scope lifecycle entries. Each runs what this scope's class does in one
  // elaboration phase, and does no tree recursion of its own -- the Runtime
  // drives the top-down walk.
  // The boundary between phases is a design-wide barrier maintained by the
  // Runtime: no scope's initialize observes any resolve mid-flight, and no
  // activate runs before every scope has initialized.
  //
  // `Resolve` executes every cross-instance route the scope
  // owns, filling each borrowed-pointer slot with the target's sealed
  // endpoint (an observable cell for a variable / net reference, an
  // alias for a `ref` port), and attaches cross-instance drivers. The
  // frontend has already proved every route has a determinate target,
  // so resolve is total: an unfilled route or type mismatch is a
  // compiler-invariant violation and surfaces here, not deferred to a
  // hot-path read.
  //
  // `Initialize` runs variable initializers and seeds driver
  // contributions; every reference across the design is sealed before
  // it starts, so an initializer observes only connected and bound
  // values.
  //
  // `CreateProcesses` creates this scope's processes.
  void Resolve();
  void Initialize();
  void CreateProcesses();

  void ForEachChild(const ChildVisitor& fn);

 private:
  // What a class extending this one does in each phase, which the entries
  // above call. The class a generated scope is of overrides all three; here
  // each does nothing. They are declared in the order the phases run, which is
  // the order they take in this class's table.
  virtual void sv_resolve();
  virtual void sv_initialize();
  virtual void sv_create_processes();

  Scope* parent_ = nullptr;
  HierarchySegment segment_;
  // Borrowed. The class this scope is a value of, which states its timescale
  // and its DPI-C exports, and which class a hierarchical name's start is of.
  const ObjectDefinition* definition_ = nullptr;
  // Physical containment: every runtime child scope this object owns
  // appears here once, in attach order. Includes anonymous scopes
  // (unnamed begin/ends).
  std::vector<std::unique_ptr<Scope>> attached_children_;
  // LRM 18.14.1 puts one on each module, interface, and program instance. A
  // generate scope is none of those and nothing draws from the one it carries.
  InitializationRng initialization_seeds_;
};

}  // namespace lyra::runtime
