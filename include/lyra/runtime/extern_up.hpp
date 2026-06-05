#pragma once

#include <string_view>

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

// An upward hierarchical reference modeled as an extern member: it holds the
// symbol -- the ancestor's instance or module name and the signal name -- and
// at Bind climbs the parent chain to bind a direct pointer to the ancestor's
// signal. Reads, writes, and sensitivity forward to that resolved cell, so the
// member behaves like the referenced `Var<T>` while naming no ancestor type
// (docs/architecture/emission_model.md). Non-movable: it registers `this`, and
// is owned by the stable scope node that declares it.
template <CaseEqualComparable T>
class ExternUp : public ExternBase {
 public:
  ExternUp(Scope* owner, std::string_view ancestor, std::string_view signal)
      : owner_(owner), ancestor_(ancestor), signal_(signal) {
    owner_->RegisterExtern(this);
  }

  void Relocate() override {
    cell_ = static_cast<Var<T>*>(owner_->ResolveUpward(ancestor_, signal_));
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
  std::string_view signal_;
  Var<T>* cell_ = nullptr;
};

}  // namespace lyra::runtime
