#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// A node in the one canonical object tree. Every constructed
// SystemVerilog scope -- a module instance, a generate block, the
// implicit `$root` -- is a Scope. It carries the scope's structural
// identity (parent plus HierarchySegment), its child scopes, and its
// registered signals. The runtime walks this same tree; there is no
// parallel topology. Every dynamic scheduler concern -- queues, process
// registry, deferred-effect attribution, ambient identity -- lives on
// the Runtime. Scope contributes only structural identity.
class Scope {
 public:
  using ChildVisitor = std::function<void(Scope&)>;

  Scope(Scope* parent, HierarchySegment segment, const ScopeProgram* program);
  virtual ~Scope() = default;
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

  // The base label of `Segment()`. Carries the source-level identifier
  // without any per-dimension index decoration; the canonical key both
  // `GetChild` and the upward visible-child climb use to match a child.
  [[nodiscard]] auto Name() const -> std::string_view;

  // Joins each ancestor's display segment with `.`, ordered from the
  // outermost named ancestor inward (LRM 21.2.1.1; `%m` resolves to this
  // string). The walk stops at the implicit `$root` so multi-top output
  // reads `Top.mid.x` rather than `$root.Top.mid.x`. Walked on demand:
  // the object tree is sealed by Activate.
  [[nodiscard]] auto HierarchicalPath() const -> lyra::value::String;

  // The scope's module definition name -- carried by every module instance,
  // empty for a generate block or `$root`. Used by dump output and debug
  // surfaces; the upward by-name climb consults `Name()` only, since slang
  // canonicalizes every head identity to a resolved symbol's name before the
  // lowering encodes the anchor.
  [[nodiscard]] auto DefName() const -> std::string_view {
    return {program_->metadata.def_name.data, program_->metadata.def_name.size};
  }

  // Records, during construction, the address of a signal this scope owns
  // under its source name. A unit answers by-name signal queries from these
  // registrations; it never inspects who asks. `name` points at an emitted
  // string literal.
  void RegisterSignal(std::string_view name, void* address);

  // Wires `child` into this scope's physical containment edge: sets
  // `child.parent_` to `this` and places `child` in the attached-children
  // relation (used by elaboration walks, dump, ForEachChild, and the
  // SV-visible by-name lookup which recurses through anonymous children).
  // Called after the typed owner commits the child, so a thrown ctor
  // leaves no half-attached scope.
  auto AddOwnedChild(std::unique_ptr<Scope> child) -> Scope*;

  // Whether this scope carries a source-visible SV name. An unnamed
  // begin/end (synthetic anonymous scope) is emitted with an empty
  // `HierarchySegment` base name; every other scope kind gets its SV
  // identifier as the base name. `GetChild` recurses through
  // non-addressable children so peer by-name lookup transparently walks
  // past them (LRM 23 hierarchical-name semantics).
  [[nodiscard]] auto IsAddressable() const -> bool {
    return !segment_.BaseName().empty();
  }

  // Returns the address of the signal registered under `name`, or the owned
  // child registered at `name` with `indices`; nullptr if none. A cross-unit
  // referrer reaches a target by name because it knows the target's type but
  // not its layout; the owner, which knows its layout, answered at construction
  // by registering the address. Resolution runs once at construction, never on
  // the simulation path.
  [[nodiscard]] auto GetSignal(std::string_view name) -> void*;
  [[nodiscard]] auto GetChild(
      std::string_view name, std::span<const lyra::value::PackedArray> indices)
      -> Scope*;

  // Walks the enclosing chain (starting at `this`) and at each level scans
  // the level's children for one whose canonical instance name plus indices
  // match. Returns the matched child itself, so the caller's descent suffix
  // is strictly below it. Matching by instance name only -- not by module
  // definition name -- is correct because the frontend has already
  // canonicalized the head per LRM 23.9 instance-name precedence; the
  // runtime does not re-implement that resolution.
  [[nodiscard]] auto ResolveVisibleChild(
      std::string_view head_name,
      std::span<const lyra::value::PackedArray> head_indices) -> Scope*;

  // Walks the enclosing chain to the topmost scope -- the parent-less `$root`
  // (LRM 23.6 absolute-path anchor) -- and returns it. The caller's descent
  // suffix is strictly below `$root`.
  [[nodiscard]] auto ResolveRoot() -> Scope*;

  // The scope's declared time precision as a power of ten (LRM Table 20-2),
  // read from its metadata; a scope with no timescale of its own reports the
  // unspecified sentinel. The engine takes the minimum across the tree to fix
  // the design-global precision (LRM 3.14.3).
  [[nodiscard]] auto TimePrecisionPower() const -> std::int8_t {
    return program_->metadata.time_precision_power;
  }

  // The scope's time unit as a power of ten (LRM Table 20-2), read from its
  // metadata: an addressable scope reports its own or inherited timescale, a
  // scope with none of its own the unspecified sentinel. Read by the DPI
  // `svGetTimeUnit` query and to scale `svGetTime` to the scope (LRM 35.5.3,
  // Annex H).
  [[nodiscard]] auto TimeUnitPower() const -> std::int8_t {
    return program_->metadata.time_unit_power;
  }

  // Per-scope lifecycle entries. Each runs this scope's generated body
  // for one elaboration phase and does no tree recursion of its own --
  // the Runtime drives the top-down walk and installs the ambient
  // current-scope for the extent of each per-scope call, so a body's
  // runtime effect (e.g. a deferred check) attributes to the scope
  // currently being walked. The boundary between phases is a
  // design-wide barrier maintained by the Runtime: no scope's initialize
  // observes any resolve mid-flight, and no activate runs before every
  // scope has initialized.
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
  struct SignalEntry {
    std::string_view name;
    void* address;
  };

  Scope* parent_ = nullptr;
  HierarchySegment segment_;
  // Borrowed. The scope's generated behavior, set at construction.
  const ScopeProgram* program_ = nullptr;
  // Physical containment: every runtime child scope this object owns
  // appears here once, in attach order. Includes anonymous scopes
  // (unnamed begin/ends). `GetChild` scans this and recurses into
  // anonymous children so SV-visible lookup ignores synthetic wrappers.
  std::vector<std::unique_ptr<Scope>> attached_children_;
  // By-name interface this scope answers cross-unit signal queries from.
  // Filled during construction; scanned only at construction-time
  // resolution, never on the simulation path.
  std::vector<SignalEntry> signals_;
};

// A module / interface / program instance: an owned child built from another
// compilation unit, reached across the unit boundary. It carries the unit
// definition; its scope program and metadata come from that definition.
class Instance : public Scope {
 public:
  Instance(
      Scope* parent, HierarchySegment segment, const UnitDefinition* definition)
      : Scope(parent, std::move(segment), &definition->root),
        definition_(definition) {
  }

  [[nodiscard]] auto Definition() const -> const UnitDefinition* {
    return definition_;
  }

 private:
  const UnitDefinition* definition_;
};

// A design-unit instance whose member storage the runtime owns, rather than a
// backend's native object layout. The definition describes the storage schema,
// the instance owns one storage object per member, and a member place resolves
// to that object's address. This is the runtime-owned counterpart of the C++
// backend's native member fields: a member is a logical place, and this is its
// realization when the backend does not lay one out physically.
class GeneratedInstance : public Instance {
 public:
  GeneratedInstance(
      Scope* parent, HierarchySegment segment, const UnitDefinition* definition)
      : Instance(parent, std::move(segment), definition) {
    members_.reserve(definition->members.size);
    for (const MemberStorageDescriptor& descriptor :
         definition->members.Descriptors()) {
      members_.push_back(std::make_unique<MemberStorage>(descriptor));
    }
  }

  [[nodiscard]] auto MemberAddress(std::uint32_t index) -> void* {
    return members_.at(index)->Address();
  }

 private:
  std::vector<std::unique_ptr<MemberStorage>> members_;
};

// A module-local generate naming scope (`if` / `for` / `case` generate block):
// an intra-unit owned child that crosses no compilation-unit boundary. It
// carries its own scope program and no unit metadata.
class GenScope : public Scope {
 public:
  using Scope::Scope;
};

// A named procedural block (`begin : outer static int x; ... end`, LRM
// 9.3.5 / 23.9) holding static-lifetime locals reachable by hierarchical
// path (`Top.outer.x`): an intra-unit owned child like `GenScope`, separate
// C++ type so the source kind survives backend dispatch and diagnostics.
class ProceduralStorageScope : public Scope {
 public:
  using Scope::Scope;
};

}  // namespace lyra::runtime
