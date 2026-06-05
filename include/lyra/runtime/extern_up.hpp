#pragma once

#include <cstddef>
#include <initializer_list>
#include <string_view>
#include <vector>

#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/var.hpp"

namespace lyra::runtime {

// A registered upward-reference member. Scope::Bind walks every ExternUp on a
// scope and relocates it once the whole object tree exists.
struct ExternBase {
  ExternBase() = default;
  ExternBase(const ExternBase&) = delete;
  auto operator=(const ExternBase&) -> ExternBase& = delete;
  ExternBase(ExternBase&&) = delete;
  auto operator=(ExternBase&&) -> ExternBase& = delete;
  virtual ~ExternBase() = default;

  virtual void Relocate() = 0;
};

// One by-name step of an extern reference's tail: an owned child of the
// resolved ancestor, named and indexed once per array dimension. Names point at
// emitted string literals; the indices it owns so the ExternUp outlives its
// constructor arguments.
struct ChildStep {
  std::string_view name;
  std::vector<std::size_t> indices;
};

// An upward hierarchical reference modeled as an extern member: it holds the
// symbol -- the ancestor's instance or module name, the by-name tail down
// through the ancestor's owned children, and the leaf signal name -- and at
// Bind climbs to the ancestor, walks the tail, and binds a direct pointer to
// the leaf. Reads, writes, and sensitivity forward to that resolved cell, so
// the member behaves like the referenced `Var<T>` while naming no ancestor
// type (docs/architecture/emission_model.md). Non-movable: it registers `this`,
// and is owned by the stable scope node that declares it.
template <CaseEqualComparable T>
class ExternUp : public ExternBase {
 public:
  ExternUp(
      Scope* owner, std::string_view ancestor,
      std::initializer_list<ChildStep> tail, std::string_view signal)
      : owner_(owner), ancestor_(ancestor), tail_(tail), signal_(signal) {
    owner_->RegisterExtern(this);
  }

  void Relocate() override {
    Scope* scope = owner_->ResolveUpwardScope(ancestor_);
    for (const ChildStep& hop : tail_) {
      scope =
          scope->GetChild(ChildRef{.name = hop.name, .indices = hop.indices});
    }
    cell_ = static_cast<Var<T>*>(scope->GetSignal(signal_));
  }

  [[nodiscard]] auto Get() const -> const T& {
    return cell_->Get();
  }

  void Set(RuntimeServices& services, const T& value) {
    cell_->Set(services, value);
  }

  auto Mutate(RuntimeServices& services) -> ScopedMutation<T> {
    return cell_->Mutate(services);
  }

  [[nodiscard]] auto AsObservable() -> Observable* {
    return cell_;
  }

 private:
  Scope* owner_;
  std::string_view ancestor_;
  std::vector<ChildStep> tail_;
  std::string_view signal_;
  Var<T>* cell_ = nullptr;
};

}  // namespace lyra::runtime
