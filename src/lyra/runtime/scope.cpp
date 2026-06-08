#include "lyra/runtime/scope.hpp"

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/extern_up.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

Scope::Scope(Scope* parent, std::string name)
    : parent_(parent), name_(std::move(name)) {
  if (parent_ != nullptr) {
    parent_->AddChild(*this);
  }
}

void Scope::AddChild(Scope& child) {
  children_.push_back(&child);
}

void Scope::ForEachChild(const ChildVisitor& fn) {
  for (Scope* child : children_) {
    fn(*child);
  }
}

auto Scope::Parent() const -> Scope* {
  return parent_;
}

auto Scope::Name() const -> std::string_view {
  return name_;
}

void Scope::Bind(RuntimeServices& services) {
  services_ = &services;
  // The whole tree is constructed before Bind runs, so every ancestor and the
  // full parent chain exist when an ExternUp member relocates by climbing.
  for (ExternBase* member : externs_) {
    member->Relocate();
  }
  CreateProcesses();
  ForEachChild([&services](Scope& child) { child.Bind(services); });
}

void Scope::RegisterExtern(ExternBase* member) {
  externs_.push_back(member);
}

auto Scope::ResolveUpwardScope(std::string_view ancestor) -> Scope* {
  Scope* s = parent_;
  while (s != nullptr && s->DefName() != ancestor) {
    s = s->Parent();
  }
  if (s == nullptr) {
    throw InternalError(
        "Scope::ResolveUpwardScope: no ancestor named " +
        std::string(ancestor) + " on the parent chain");
  }
  return s;
}

auto Scope::Services() -> RuntimeServices& {
  if (services_ == nullptr) {
    throw InternalError("Scope::Services: scope not bound");
  }
  return *services_;
}

auto Scope::AddProcess(ProcessKind kind, Coroutine coroutine)
    -> RuntimeProcess& {
  processes_.push_back(
      std::make_unique<RuntimeProcess>(kind, std::move(coroutine)));
  return *processes_.back();
}

void Scope::SubmitObserved(std::uint32_t site_id, std::function<void()> fn) {
  if (site_id >= observed_pending_.size()) {
    observed_pending_.resize(site_id + 1);
  }
  observed_pending_[site_id] = std::move(fn);
}

void Scope::DrainObserved() {
  for (auto& fn : observed_pending_) {
    if (fn) {
      fn();
      fn = nullptr;
    }
  }
}

}  // namespace lyra::runtime
