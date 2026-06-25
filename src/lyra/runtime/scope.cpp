#include "lyra/runtime/scope.hpp"

#include <cstddef>
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
  // Linking establishes the full bidirectional edge. A child built under a
  // parent already set `parent_` in its constructor; a top-level block is
  // constructed parentless and adopts `$root` as its parent here, so an
  // upward climb from inside a top reaches the root (LRM 23.6).
  child.parent_ = this;
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
    std::string_view name, std::span<const lyra::value::PackedArray> indices,
    Scope& child) {
  std::vector<std::size_t> host_indices;
  host_indices.reserve(indices.size());
  for (const auto& idx : indices) {
    host_indices.push_back(static_cast<std::size_t>(idx.ToInt64()));
  }
  child_entries_.push_back(
      ChildEntry{
          .name = name, .indices = std::move(host_indices), .scope = &child});
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
    std::string_view name, std::span<const lyra::value::PackedArray> indices)
    -> Scope* {
  for (const ChildEntry& child : child_entries_) {
    if (child.name != name || child.indices.size() != indices.size()) {
      continue;
    }
    bool matched = true;
    for (std::size_t i = 0; i < indices.size(); ++i) {
      if (child.indices[i] != static_cast<std::size_t>(indices[i].ToInt64())) {
        matched = false;
        break;
      }
    }
    if (matched) {
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

void Scope::Resolve() {
  // The whole tree is constructed before resolution runs, so every ancestor and
  // the full parent chain exist when an ExternUp member relocates by climbing,
  // and every child exists when this scope binds a child's `ref` port. The walk
  // is top-down, so a parent binds a child's reference before the child (or its
  // own children) forwards it onward.
  ResolveState();
  for (ExternBase* member : externs_) {
    member->Relocate();
  }
  ForEachChild([](Scope& child) { child.Resolve(); });
}

void Scope::Initialize() {
  InitializeState();
  ForEachChild([](Scope& child) { child.Initialize(); });
}

void Scope::Activate() {
  CreateProcesses();
  ForEachChild([](Scope& child) { child.Activate(); });
}

void Scope::RegisterExtern(ExternBase* member) {
  externs_.push_back(member);
}

auto Scope::ResolveUpwardScope(std::string_view key, UpwardMatch match)
    -> Scope* {
  for (Scope* s = parent_; s != nullptr; s = s->Parent()) {
    const std::string_view name =
        match == UpwardMatch::kDefName ? s->DefName() : s->Name();
    if (name == key) {
      return s;
    }
  }
  throw InternalError(
      "Scope::ResolveUpwardScope: no ancestor named " + std::string(key) +
      " on the parent chain");
}

auto Scope::Services() -> RuntimeServices& {
  return *services_;
}

void Scope::RegisterInitial(Coroutine<void> coroutine) {
  processes_.push_back(
      std::make_unique<RuntimeProcess>(
          ProcessKind::kInitial, std::move(coroutine)));
}

void Scope::RegisterFinal(Coroutine<void> coroutine) {
  processes_.push_back(
      std::make_unique<RuntimeProcess>(
          ProcessKind::kFinal, std::move(coroutine)));
}

void Scope::SubmitObserved(
    const lyra::value::PackedArray& site_id, std::function<void()> fn) {
  const auto slot = static_cast<std::size_t>(site_id.ToInt64());
  if (slot >= observed_pending_.size()) {
    observed_pending_.resize(slot + 1);
  }
  observed_pending_[slot] = std::move(fn);
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
