#include "lyra/runtime/scope.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/extern_up.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

Scope::Scope(Scope* parent, std::string name, RuntimeServices& services)
    : parent_(parent), name_(std::move(name)), services_(&services) {
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

void Scope::RegisterSignal(std::string_view name, void* address) {
  signals_.push_back(SignalEntry{.name = name, .address = address});
}

void Scope::RegisterChild(
    std::string_view name, std::span<const std::size_t> indices, Scope& child) {
  child_entries_.push_back(
      ChildEntry{
          .name = name,
          .indices = std::vector<std::size_t>(indices.begin(), indices.end()),
          .scope = &child});
}

auto Scope::GetSignal(std::string_view name) -> void* {
  for (const SignalEntry& signal : signals_) {
    if (signal.name == name) {
      return signal.address;
    }
  }
  return nullptr;
}

auto Scope::GetChild(
    std::string_view name, std::span<const std::size_t> indices) -> Scope* {
  for (const ChildEntry& child : child_entries_) {
    if (child.name == name && std::ranges::equal(child.indices, indices)) {
      return child.scope;
    }
  }
  return nullptr;
}

auto Scope::Parent() const -> Scope* {
  return parent_;
}

auto Scope::Name() const -> std::string_view {
  return name_;
}

void Scope::Bind() {
  // The whole tree is constructed before Bind runs, so every ancestor and the
  // full parent chain exist when an ExternUp member relocates by climbing.
  for (ExternBase* member : externs_) {
    member->Relocate();
  }
  CreateProcesses();
  ForEachChild([](Scope& child) { child.Bind(); });
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
