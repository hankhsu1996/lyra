#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/class_value.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class CancellationTarget;

// A node in the one canonical object tree. Every constructed
// SystemVerilog scope -- a module instance, a generate block, the
// implicit `$root` -- is a Scope.
//
// It is a value of a class like any other, which is where what it is of and the
// storage that brings come from; what this kind adds is its place in the tree
// (parent plus HierarchySegment, its child scopes, its registered signals) and
// the program the runtime drives it through. The runtime walks this same tree;
// there is no parallel topology. Every dynamic scheduler concern -- queues,
// process registry, deferred-effect attribution, ambient identity -- lives on
// the Runtime, so beyond being a value of its class a scope contributes
// structural identity and nothing else.
class Scope : public ClassValue {
 public:
  using ChildVisitor = std::function<void(Scope&)>;

  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one.
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

  // The base label of `Segment()`. Carries the source-level identifier
  // without any per-dimension index decoration; the canonical key every
  // by-name reach for a child matches on, downward and on the upward climb
  // alike.
  [[nodiscard]] auto Name() const -> std::string_view;

  // Joins each ancestor's display segment with `.`, ordered from the outermost
  // inward (LRM 21.2.1.5; `%m` resolves to this string). A scope the source
  // never named contributes nothing, which is what keeps it off every
  // hierarchical path (LRM 23.6) without taking it out of the tree. The walk
  // stops at the implicit `$root` so multi-top output reads `Top.mid.x` rather
  // than `$root.Top.mid.x`. Walked on demand: the object tree is sealed by
  // Activate.
  [[nodiscard]] auto HierarchicalPath() const -> lyra::value::String;

  // The generated behavior this scope was built with. The runtime drives the
  // lifecycle entries itself, so this is how a caller reaches a behavior the
  // scope publishes for someone outside the lifecycle to call.
  [[nodiscard]] auto Program() const -> const ScopeProgram& {
    return *program_;
  }

  // Records, during construction, the address of a signal this scope owns
  // under its source name. A scope answers by-name signal queries from these
  // registrations; it never inspects who asks. `name` points at an emitted
  // string literal.
  void RegisterSignal(std::string_view name, void* address);

  // Records, during construction, what a `disable` naming this scope terminates
  // (LRM 9.6.2). The cell lives wherever the scope's other static-lifetime
  // state lives, so what the scope keeps is its address. A scope has exactly
  // one, which is why this takes no name: reaching the scope is the whole of
  // naming what a `disable` there ends.
  void RegisterDisableTarget(CancellationTarget* target);

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
  // identifier as the base name. A by-name reach recurses through
  // non-addressable children, so it walks past them transparently (LRM 23
  // hierarchical-name semantics).
  [[nodiscard]] auto IsAddressable() const -> bool {
    return !segment_.BaseName().empty();
  }

  // What this scope answers with: the cell of a signal it registered, the owned
  // child at that name and indices, the entry of a subroutine it declares with
  // its prototype erased, or what a `disable` naming this scope terminates. A
  // cross-unit referrer reaches a target this way because it knows the target's
  // type but not its layout; the owner, which knows its layout, answered at
  // construction by registering it. Resolution runs once at construction, never
  // on the simulation path.
  //
  // All four throw where the scope has no such answer. What reaches here was
  // resolved to a declaration of this scope before anything was emitted for it,
  // so absence is not a state a legal program reaches -- and handing back
  // nothing instead would put the failure at whatever dereferences the answer,
  // which names neither the scope nor what was asked of it.
  [[nodiscard]] auto FindSignal(std::string_view name) -> void*;
  [[nodiscard]] auto FindChild(
      std::string_view name, std::span<const lyra::value::PackedArray> indices)
      -> Scope*;
  [[nodiscard]] auto FindSubroutine(std::string_view name)
      -> ErasedScopeCallable;
  [[nodiscard]] auto FindDisableTarget() -> CancellationTarget*;

  // The definition of a class this scope's unit declares. Such a class is a
  // distinct type per instance of the element declaring it (LRM 6.22), so which
  // definition a name reaches is this scope's to answer and no referrer's to
  // assume. Asked while a reference resolves, never on the simulation path.
  [[nodiscard]] auto FindClass(std::string_view name)
      -> const ObjectDefinition*;

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
  // the Runtime drives the top-down walk. The boundary between phases is
  // a design-wide barrier maintained by the Runtime: no scope's
  // initialize observes any resolve mid-flight, and no activate runs
  // before every scope has initialized.
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

 protected:
  // A scope the runtime lays out is made with room for its members after it,
  // so only the allocation that makes that room builds one; a target that lays
  // its own scopes out builds one as the base of its own class.
  Scope(
      Scope* parent, HierarchySegment segment,
      const ScopeDefinition* definition);

 private:
  friend class ClassValue;

  struct SignalEntry {
    std::string_view name;
    void* address;
  };

  // The one place a name is looked for rather than reached: the climb tries
  // each enclosing level in turn, so "not at this level" is an answer there
  // and nowhere else.
  [[nodiscard]] auto LookupChild(
      std::string_view name, std::span<const lyra::value::PackedArray> indices)
      -> Scope*;

  // One message shape for every name this scope cannot answer.
  [[nodiscard]] auto NoSuchName(
      std::string_view what, std::string_view name) const -> std::string;

  Scope* parent_ = nullptr;
  HierarchySegment segment_;
  // Borrowed. How the runtime drives an instance of the class this scope is,
  // taken from that class at construction. What class it is, is what every
  // value of one carries; this is the half that exists because the runtime
  // enters this kind of value rather than only dispatching on it.
  const ScopeProgram* program_ = nullptr;
  // Physical containment: every runtime child scope this object owns
  // appears here once, in attach order. Includes anonymous scopes
  // (unnamed begin/ends). A by-name reach scans this and recurses into
  // anonymous children, so SV-visible lookup ignores synthetic wrappers.
  std::vector<std::unique_ptr<Scope>> attached_children_;
  // By-name interface this scope answers cross-unit signal queries from.
  // Filled during construction; scanned only at construction-time
  // resolution, never on the simulation path.
  std::vector<SignalEntry> signals_;
  // Borrowed. What a `disable` naming this scope terminates (LRM 9.6.2), set
  // during construction by whoever owns the cell. Null on a scope the source
  // named nothing, which no hierarchical name reaches either.
  CancellationTarget* disable_target_ = nullptr;
  // LRM 18.14.1 puts one on each module, interface, and program instance. A
  // generate scope is none of those and nothing draws from the one it carries.
  InitializationRng initialization_seeds_;
};

}  // namespace lyra::runtime
