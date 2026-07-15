#include "lyra/runtime/scope.hpp"

#include <cstddef>
#include <functional>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

// The default scope-program entry: a scope with no work for a lifecycle phase
// keeps this no-op, which ignores the scope it is handed.
// NOLINTNEXTLINE(readability-named-parameter)
void ScopeNoOp(Scope*) {
}

Scope::Scope(
    Scope* parent, HierarchySegment segment, RuntimeServices& services,
    const ScopeProgram* program)
    : parent_(parent),
      segment_(std::move(segment)),
      services_(&services),
      program_(program) {
}

auto Scope::AddOwnedChild(std::unique_ptr<Scope> child) -> Scope* {
  child->parent_ = this;
  Scope* handle = child.get();
  attached_children_.push_back(std::move(child));
  return handle;
}

void Scope::ForEachChild(const ChildVisitor& fn) {
  for (const auto& child : attached_children_) {
    fn(*child);
  }
}

void Scope::RegisterSignal(std::string_view name, void* address) {
  signals_.push_back(SignalEntry{.name = name, .address = address});
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
  // SV-visible child lookup. Named children match on segment name +
  // indices; anonymous children (unnamed begin/ends emitted with an empty
  // segment name) are transparent -- the walk recurses into them so a
  // peer's `top.outer.x` finds `outer` regardless of how many unnamed
  // begin/ends physically wrap it (LRM 23 hierarchical-name semantics).
  for (const auto& child : attached_children_) {
    if (!child->IsAddressable()) {
      if (Scope* found = child->GetChild(name, indices)) {
        return found;
      }
      continue;
    }
    const HierarchySegment& seg = child->segment_;
    if (seg.BaseName() != name) {
      continue;
    }
    const auto child_indices = seg.Indices();
    if (child_indices.size() != indices.size()) {
      continue;
    }
    bool matched = true;
    for (std::size_t i = 0; i < indices.size(); ++i) {
      if (child_indices[i].ToInt64() != indices[i].ToInt64()) {
        matched = false;
        break;
      }
    }
    if (matched) {
      return child.get();
    }
  }
  return nullptr;
}

auto Scope::Parent() const -> Scope* {
  return parent_;
}

auto Scope::Segment() const -> const HierarchySegment& {
  return segment_;
}

auto Scope::DisplaySegment() const -> std::string {
  return segment_.Display();
}

auto Scope::Name() const -> std::string_view {
  return segment_.BaseName();
}

auto Scope::HierarchicalPath() const -> lyra::value::String {
  // Each segment is the scope's own LRM display form (carrying any per-dim
  // bracketed index it acquired at construction). Stopping at
  // `parent_ == nullptr` drops the implicit `$root` from the joined output
  // -- the root anchors top-level adoption but is not part of a
  // user-visible hierarchical path.
  std::vector<std::string> parts;
  for (const Scope* cur = this; cur != nullptr && cur->parent_ != nullptr;
       cur = cur->parent_) {
    parts.push_back(cur->segment_.Display());
  }
  std::string out;
  for (const std::string& part : std::views::reverse(parts)) {
    if (!out.empty()) {
      out.push_back('.');
    }
    out.append(part);
  }
  return lyra::value::String(std::move(out));
}

void Scope::Resolve() {
  // The whole tree is constructed before resolution runs, so every ancestor and
  // the full parent chain exist when this scope's `ResolveState` walks the
  // route of each cross-instance reference it owns. The walk is top-down, so a
  // parent binds a child's `ref` port before the child (or its own children)
  // forwards it onward.
  {
    GeneratedCallScope call;
    program_->resolve_state(this);
  }
  ForEachChild([](Scope& child) { child.Resolve(); });
}

void Scope::Initialize() {
  {
    GeneratedCallScope call;
    program_->initialize_state(this);
  }
  ForEachChild([](Scope& child) { child.Initialize(); });
}

void Scope::Activate() {
  {
    GeneratedCallScope call;
    program_->create_processes(this);
  }
  ForEachChild([](Scope& child) { child.Activate(); });
}

auto Scope::ResolveVisibleChild(
    std::string_view head_name,
    std::span<const lyra::value::PackedArray> head_indices) -> Scope* {
  for (Scope* level = this; level != nullptr; level = level->Parent()) {
    if (Scope* child = level->GetChild(head_name, head_indices)) {
      return child;
    }
  }
  throw InternalError(
      "Scope::ResolveVisibleChild: no child named " + std::string(head_name) +
      " visible from this scope's enclosing chain");
}

auto Scope::ResolveRoot() -> Scope* {
  Scope* level = this;
  while (level->parent_ != nullptr) {
    level = level->parent_;
  }
  return level;
}

auto Scope::Services() -> RuntimeServices& {
  return *services_;
}

void Scope::RegisterInitial(Coroutine<void> coroutine) {
  processes_.push_back(
      std::make_shared<RuntimeProcess>(
          ProcessKind::kInitial, std::move(coroutine)));
}

void Scope::RegisterFinal(Coroutine<void> coroutine) {
  processes_.push_back(
      std::make_shared<RuntimeProcess>(
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
