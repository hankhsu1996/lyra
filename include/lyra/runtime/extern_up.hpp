#pragma once

#include <span>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// A registered upward-reference member. Scope walks every ExternUp on a scope
// and relocates it once the whole object tree exists.
struct ExternBase {
  ExternBase() = default;
  ExternBase(const ExternBase&) = delete;
  auto operator=(const ExternBase&) -> ExternBase& = delete;
  ExternBase(ExternBase&&) = delete;
  auto operator=(ExternBase&&) -> ExternBase& = delete;
  virtual ~ExternBase() = default;

  virtual void Relocate() = 0;
};

// An upward hierarchical reference modeled as an extern member. Default-
// constructed in the field declaration like any other wrapper-typed member;
// the per-reference state arrives through ordinary method calls in the
// owning scope's constructor body. At the resolve phase the wrapper climbs
// and descends, fetching a typed pointer to the leaf cell; reads, writes,
// and sensitivity then forward to that cell while the member names no
// ancestor type.
//
// `lookup_origin` is passed explicitly to every bind method rather than
// taken as `this`: the navigation starts from the scope where the SV
// hierarchical reference is lexically evaluated, which a caller may need to
// pass independently of where the wrapper is stored.
//
// String arguments (`head_name`, `signal`, suffix step `name`) are
// `string_view`s the wrapper stores by reference; their storage must outlive
// the wrapper. The emitted code supplies static string literals, which
// satisfies the requirement. PackedArray index spans are copied into
// wrapper-owned vectors at the call.
template <value::LyraValue T>
class ExternUp : public ExternBase {
 public:
  ExternUp() = default;

  // Install the by-name climb start: at bind, walk the lookup origin's
  // enclosing chain and find the child whose canonical instance name (and
  // per-dimension index) matches `head_name` + `head_indices`. `signal` is
  // the leaf cell's registered name on the matched scope.
  void BindVisibleChild(
      Scope* lookup_origin, std::string_view head_name,
      std::span<const value::PackedArray> head_indices,
      std::string_view signal) {
    lookup_origin_ = lookup_origin;
    is_root_ = false;
    head_name_ = head_name;
    head_indices_.assign(head_indices.begin(), head_indices.end());
    signal_ = signal;
    lookup_origin_->RegisterExtern(this);
  }

  // Install the `$root` anchor: at bind, walk to the parent-less topmost
  // scope. `signal` is the leaf cell's registered name on the scope reached
  // after the suffix walks down.
  void BindRoot(Scope* lookup_origin, std::string_view signal) {
    lookup_origin_ = lookup_origin;
    is_root_ = true;
    signal_ = signal;
    lookup_origin_->RegisterExtern(this);
  }

  // Append one descent step below the anchor, applied in call order. Each
  // step is a named child of the previous scope, optionally per-dimension
  // indexed.
  void AddSuffixStep(
      std::string_view name, std::span<const value::PackedArray> indices) {
    SuffixStep step;
    step.name = name;
    step.indices.assign(indices.begin(), indices.end());
    suffix_.push_back(std::move(step));
  }

  void Relocate() override {
    Scope* scope = is_root_ ? lookup_origin_->ResolveRoot()
                            : lookup_origin_->ResolveVisibleChild(
                                  head_name_, head_indices_);
    for (const SuffixStep& step : suffix_) {
      scope = scope->GetChild(step.name, step.indices);
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
  struct SuffixStep {
    std::string_view name;
    std::vector<value::PackedArray> indices;
  };

  Scope* lookup_origin_ = nullptr;
  bool is_root_ = false;
  std::string_view head_name_;
  std::vector<value::PackedArray> head_indices_;
  std::string_view signal_;
  std::vector<SuffixStep> suffix_;
  Var<T>* cell_ = nullptr;
};

}  // namespace lyra::runtime
