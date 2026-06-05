#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

class RuntimeServices;
class ExternBase;

// Returned by a scope that declares no timescale of its own (the synthetic
// `$root`). The engine's design-global precision minimum ignores it, so a
// purely structural node does not pull the simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePrecisionPower = 127;

// A node in the one canonical object tree. Every constructed SystemVerilog
// scope -- a module instance, a generate block, the implicit `$root` -- is a
// Scope. It carries the scope's identity (name, parent), the services handle
// its bodies reach through, its own processes, and the observed-region pending
// closures. The scheduler walks this same tree; there is no parallel topology.
class Scope {
 public:
  using ChildVisitor = std::function<void(Scope&)>;

  Scope(Scope* parent, std::string name);
  virtual ~Scope() = default;
  Scope(const Scope&) = delete;
  auto operator=(const Scope&) -> Scope& = delete;
  Scope(Scope&&) = delete;
  auto operator=(Scope&&) -> Scope& = delete;

  [[nodiscard]] auto Parent() const -> Scope*;
  [[nodiscard]] auto Name() const -> std::string_view;

  // The scope's module definition name. An upward hierarchical reference climbs
  // the parent chain matching its first name component against each ancestor's
  // instance name (`Name()`) or module name (this), since the LRM lets the
  // first component be either (LRM 23.8). The base never matches; a generated
  // unit class overrides it.
  [[nodiscard]] virtual auto DefName() const -> std::string_view {
    return {};
  }

  // Returns the address of a signal this scope owns by that name, or nullptr if
  // it has none. An upward reference cannot name its ancestor's type, so it
  // fetches the signal by name and the ancestor's own class answers
  // (docs/architecture/emission_model.md).
  // NOLINTNEXTLINE(readability-named-parameter)
  [[nodiscard]] virtual auto GetSignal(std::string_view) -> void* {
    return nullptr;
  }

  // Climbs the parent chain to the ancestor whose instance name (`Name()`) or
  // module name (`DefName()`) is `ancestor` and returns its signal named
  // `signal` (LRM 23.8). The single shared implementation of an upward
  // reference's runtime navigation; an `ExternUp` member calls it once at Bind.
  [[nodiscard]] auto ResolveUpward(
      std::string_view ancestor, std::string_view signal) -> void*;

  // An `ExternUp` member registers itself here from its constructor; Bind
  // relocates all registered members once the whole tree exists.
  void RegisterExtern(ExternBase* member);

  // The scope's declared time precision as a power of ten (LRM Table 20-2).
  // A scope that declares a timescale overrides this; the base returns the
  // unspecified sentinel. The engine takes the minimum across the tree to fix
  // the design-global precision (LRM 3.14.3).
  [[nodiscard]] virtual auto TimePrecisionPower() const -> std::int8_t {
    return kUnspecifiedTimePrecisionPower;
  }

  // Wires the services handle, creates this scope's processes, then recurses
  // into the children. Identity (name, parent, kind-as-type) is already fixed
  // at construction; bind only installs runtime behavior.
  void Bind(RuntimeServices& services);

  // Last-write-wins per site within a time slot: re-submit at the same site
  // overwrites the prior closure, which suppresses settle-time glitches.
  void SubmitObserved(std::uint32_t site_id, std::function<void()> fn);
  void DrainObserved();

  // Links a child into this scope. Owned children register themselves here
  // from their constructor; the engine adds the top-level blocks to `$root`.
  void AddChild(Scope& child);
  void ForEachChild(const ChildVisitor& fn);

  template <typename Fn>
  void ForEachProcess(Fn&& fn) {
    auto&& f = std::forward<Fn>(fn);
    for (auto& process : processes_) {
      f(*process);
    }
  }

 protected:
  auto AddProcess(ProcessKind kind, Coroutine coroutine) -> RuntimeProcess&;

  // Reached by emitted process and subroutine bodies for every runtime
  // service call (print, delay, NBA submit, file I/O, the deferred-check
  // observed submit).
  [[nodiscard]] auto Services() -> RuntimeServices&;

 private:
  // A scope with no processes creates none; only scopes with processes
  // override this.
  virtual void CreateProcesses() {
  }

  Scope* parent_ = nullptr;
  std::string name_;
  RuntimeServices* services_ = nullptr;
  // Non-owning child links; the typed members on the derived class own the
  // children. This is the object tree's own structure, not a parallel one.
  std::vector<Scope*> children_;
  std::vector<std::unique_ptr<RuntimeProcess>> processes_;
  // Empty std::function == clean slot; no parallel dirty bitmap needed.
  std::vector<std::function<void()>> observed_pending_;
  // Non-owning links to this scope's ExternUp members; relocated at Bind once
  // the whole tree exists. The members are owned by the derived class.
  std::vector<ExternBase*> externs_;
};

// A module / interface / program instance: an owned child built from another
// compilation unit, reached across the unit boundary
// (hierarchy_and_generate.md).
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
