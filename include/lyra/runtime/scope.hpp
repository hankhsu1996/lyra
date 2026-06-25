#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;
class ExternBase;

// Returned by a scope that declares no timescale of its own (the synthetic
// `$root`). The engine's design-global precision minimum ignores it, so a
// purely structural node does not pull the simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePrecisionPower = 127;

// How an upward hierarchical reference matches its ancestor while climbing the
// parent chain (LRM 23.8): against the ancestor's module definition name
// (`DefName()`, a module-instance head) or against the scope's own segment
// base name (`Segment().BaseName()`, a named generate block or `$root`).
enum class UpwardMatch : std::uint8_t { kDefName, kScopeName };

// A node in the one canonical object tree. Every constructed SystemVerilog
// scope -- a module instance, a generate block, the implicit `$root` -- is a
// Scope. It carries the scope's structural identity (parent plus
// HierarchySegment), the services handle its bodies reach through, its own
// processes, and the observed-region pending closures. The scheduler walks
// this same tree; there is no parallel topology.
class Scope {
 public:
  using ChildVisitor = std::function<void(Scope&)>;

  Scope(Scope* parent, HierarchySegment segment, RuntimeServices& services);
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
  // without any per-dimension index decoration; used by cross-unit by-name
  // lookup keys and by upward `kScopeName` resolution.
  [[nodiscard]] auto Name() const -> std::string_view;

  // Joins each ancestor's display segment with `.`, ordered from the
  // outermost named ancestor inward (LRM 21.2.1.1; `%m` resolves to this
  // string). The walk stops at the implicit `$root` so multi-top output
  // reads `Top.mid.x` rather than `$root.Top.mid.x`. Walked on demand:
  // the object tree is sealed by Activate.
  [[nodiscard]] auto HierarchicalPath() const -> lyra::value::String;

  // The scope's module definition name. An upward hierarchical reference climbs
  // the parent chain matching against this (LRM 23.8): the referrer's emitted
  // code keys the climb on the target's definition name, so an ancestor matches
  // by its definition name, never by its instance name. The base never matches;
  // a generated unit class overrides it.
  [[nodiscard]] virtual auto DefName() const -> std::string_view {
    return {};
  }

  // Records, during construction, the address of a signal this scope owns
  // under its source name. A unit answers by-name signal queries from these
  // registrations; it never inspects who asks. `name` points at an emitted
  // string literal.
  void RegisterSignal(std::string_view name, void* address);

  // Wires `child` into this scope's object-tree edge: sets `child.parent_`
  // to `this`, places `child` in the attached-children relation, and makes
  // it findable through `GetChild` (key derived from `child.Segment()`).
  // One write point per child edge; the parent never re-states identity
  // the child already holds. Called after the typed owner commits the
  // child, so a thrown ctor leaves no half-attached scope.
  void AttachChild(Scope& child);

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

  // Climbs the parent chain to the nearest ancestor named `key` and returns it
  // (LRM 23.8). `match` selects the name compared: `kDefName` matches an
  // ancestor's module definition name (`DefName()`, a module-instance head), so
  // an ancestor whose instance name merely equals it does not match;
  // `kScopeName` matches the scope's own name (`Name()`, a named generate block
  // or `$root`), whose name is itself the lookup key. The shared start of an
  // upward reference's runtime navigation; from there an `ExternUp` member
  // walks any by-name tail and fetches the leaf, once in the resolve phase.
  [[nodiscard]] auto ResolveUpwardScope(std::string_view key, UpwardMatch match)
      -> Scope*;

  // An `ExternUp` member registers itself here from its constructor; the
  // resolve phase relocates all registered members once the whole tree exists.
  void RegisterExtern(ExternBase* member);

  // The scope's declared time precision as a power of ten (LRM Table 20-2).
  // A scope that declares a timescale overrides this; the base returns the
  // unspecified sentinel. The engine takes the minimum across the tree to fix
  // the design-global precision (LRM 3.14.3).
  [[nodiscard]] virtual auto TimePrecisionPower() const -> std::int8_t {
    return kUnspecifiedTimePrecisionPower;
  }

  // The post-construction elaboration phases, each a top-down walk over the
  // whole subtree. Services and structure are already wired in the constructor;
  // these install behavior that needs the whole tree to exist. They run in
  // order across the design: every scope resolves, then every scope
  // initializes, then every scope activates.
  //
  // Resolve relocates registered `ExternUp` members (cross-tree references).
  // Initialize runs variable initializers, after resolution, so an initializer
  // observes connected and bound values. Activate creates this scope's
  // processes, after initialization.
  void Resolve();
  void Initialize();
  void Activate();

  // Last-write-wins per site within a time slot: re-submit at the same site
  // overwrites the prior closure, which suppresses settle-time glitches. The
  // site id is the compile-time-fixed slot index passed as an SV integer; the
  // runtime projects to `std::size_t` internally at the API boundary.
  void SubmitObserved(
      const lyra::value::PackedArray& site_id, std::function<void()> fn);
  void DrainObserved();

  void ForEachChild(const ChildVisitor& fn);

  template <typename Fn>
  void ForEachProcess(Fn&& fn) {
    auto&& f = std::forward<Fn>(fn);
    for (auto& process : processes_) {
      f(*process);
    }
  }

 protected:
  // Reached by the emitted `CreateProcesses` body to bind a process coroutine
  // to this scope's startup (`RegisterInitial`, LRM 9.2) or shutdown
  // (`RegisterFinal`, LRM 9.2.3) lifecycle. Distinct entries, not one kind-
  // tagged call, mirroring the two MIR registration callees.
  void RegisterInitial(Coroutine<void> coroutine);
  void RegisterFinal(Coroutine<void> coroutine);

  // Reached by emitted process and subroutine bodies for every runtime
  // service call (print, delay, NBA submit, file I/O, the deferred-check
  // observed submit).
  [[nodiscard]] auto Services() -> RuntimeServices&;

 private:
  // A scope with no cross-instance bindings does none; only scopes that bind a
  // child's reference (a `ref` port) override this. Called in the resolve
  // phase, once the whole tree is constructed.
  virtual void ResolveState() {
  }

  // A scope with no variable initializers runs none; only scopes with
  // initializers override this. Called in the initialize phase, after the whole
  // tree has resolved its references.
  virtual void InitializeState() {
  }

  // A scope with no processes creates none; only scopes with processes
  // override this.
  virtual void CreateProcesses() {
  }

  struct SignalEntry {
    std::string_view name;
    void* address;
  };

  Scope* parent_ = nullptr;
  HierarchySegment segment_;
  // Borrowed; set in the constructor.
  RuntimeServices* services_ = nullptr;
  // The single object-tree edge: every child this scope owns appears here
  // once, with its `Segment()` carrying both the LRM display name and the
  // by-name lookup key. Traversal walks this in deterministic attach order;
  // by-name lookup scans it and compares each child's segment.
  std::vector<Scope*> attached_children_;
  // By-name interface this scope answers cross-unit signal queries from.
  // Filled during construction; scanned only at construction-time
  // resolution, never on the simulation path.
  std::vector<SignalEntry> signals_;
  std::vector<std::unique_ptr<RuntimeProcess>> processes_;
  // Empty std::function == clean slot; no parallel dirty bitmap needed.
  std::vector<std::function<void()>> observed_pending_;
  // Non-owning links to this scope's ExternUp members; relocated in the resolve
  // phase once the whole tree exists. The members are owned by the derived
  // class.
  std::vector<ExternBase*> externs_;
};

// A module / interface / program instance: an owned child built from another
// compilation unit, reached across the unit boundary.
class Instance : public Scope {
 public:
  using Scope::Scope;
};

// A module-local generate naming scope (`if` / `for` / `case` generate block):
// an intra-unit owned child that crosses no compilation-unit boundary.
class GenScope : public Scope {
 public:
  using Scope::Scope;
};

}  // namespace lyra::runtime
