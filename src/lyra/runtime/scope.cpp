#include "lyra/runtime/scope.hpp"

#include <format>
#include <memory>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/class_definition.hpp"

namespace lyra::runtime {

Scope::Scope() = default;

Scope::~Scope() = default;

void Scope::SetPlace(Scope* parent, HierarchySegment segment) {
  parent_ = parent;
  segment_ = std::move(segment);
}

auto Scope::Attach(OwnedObject<Scope> child) -> Scope* {
  Scope* parent = child->parent_;
  if (parent == nullptr) {
    throw InternalError(
        "Scope::Attach: a scope was attached that was placed under no parent "
        "-- please report this as a bug");
  }
  Scope* handle = child.get();
  parent->attached_children_.push_back(std::move(child));
  return handle;
}

void Scope::ForEachChild(const ChildVisitor& fn) {
  for (const auto& child : attached_children_) {
    fn(*child);
  }
}

auto Scope::Segment() const -> const HierarchySegment& {
  return segment_;
}

auto Scope::DisplaySegment() const -> std::string {
  return segment_.Display();
}

auto Scope::HierarchicalPath() const -> lyra::value::String {
  return HierarchicalPathOf(lyra::value::String(""));
}

auto Scope::HierarchicalPathOf(const lyra::value::String& below) const
    -> lyra::value::String {
  // Each segment is the scope's own LRM display form (carrying any per-dim
  // bracketed index it acquired at construction). Stopping at
  // `parent_ == nullptr` drops the implicit `$root` from the joined output
  // -- the root anchors top-level adoption but is not part of a
  // user-visible hierarchical path.
  //
  // What `below` names is innermost, and like a scope with no name it adds
  // nothing where it names nothing.
  std::vector<std::string> parts;
  if (!below.View().empty()) {
    parts.emplace_back(below.View());
  }
  for (const Scope* cur = this; cur != nullptr && cur->parent_ != nullptr;
       cur = cur->parent_) {
    if (!cur->IsAddressable()) {
      continue;
    }
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

void Scope::Enter(abi::ScopePhase abi::ScopeInfo::* phase) {
  (Info().*phase)(this);
}

void Scope::Resolve() {
  Enter(&abi::ScopeInfo::resolve);
}

void Scope::Initialize() {
  Enter(&abi::ScopeInfo::initialize);
}

void Scope::CreateProcesses() {
  Enter(&abi::ScopeInfo::create_processes);
}

void Scope::Describe() {
  Enter(&abi::ScopeInfo::describe);
}

auto Scope::EnclosingScope(const abi::ObjectDefinition* cls) -> Scope* {
  for (Scope* level = parent_; level != nullptr; level = level->parent_) {
    if (IsOfClass(level->definition, cls)) return level;
    if (level->parent_ != nullptr) continue;
    for (const auto& top : level->attached_children_) {
      if (IsOfClass(top->definition, cls)) return top.get();
    }
  }
  throw InternalError(
      std::format(
          "Scope::EnclosingScope: '{}' stands in no scope of the class its "
          "name was resolved against",
          HierarchicalPath().CStr()));
}

auto Scope::InitializationSeeds() -> InitializationRng& {
  return initialization_seeds_;
}

}  // namespace lyra::runtime
