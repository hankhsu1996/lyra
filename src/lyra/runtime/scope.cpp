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

Scope::Scope(
    Scope* parent, HierarchySegment segment, const ObjectDefinition* definition)
    : parent_(parent),
      segment_(std::move(segment)),
      definition_(RequireScopeClass(definition)) {
}

Scope::~Scope() = default;

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

auto Scope::Parent() const -> Scope* {
  return parent_;
}

auto Scope::Segment() const -> const HierarchySegment& {
  return segment_;
}

auto Scope::DisplaySegment() const -> std::string {
  return segment_.Display();
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

void Scope::Resolve() {
  sv_resolve();
}

void Scope::Initialize() {
  sv_initialize();
}

void Scope::CreateProcesses() {
  sv_create_processes();
}

void Scope::sv_resolve() {
}

void Scope::sv_initialize() {
}

void Scope::sv_create_processes() {
}

namespace {

auto IsOfClass(const Scope& scope, const ObjectDefinition* cls) -> bool {
  for (const ObjectDefinition* at = scope.Definition(); at != nullptr;
       at = at->base) {
    if (at == cls) return true;
  }
  return false;
}

}  // namespace

auto Scope::EnclosingInstance(const ObjectDefinition* cls) -> Scope* {
  for (Scope* level = parent_; level != nullptr; level = level->parent_) {
    if (IsOfClass(*level, cls)) return level;
    if (level->parent_ != nullptr) continue;
    for (const auto& top : level->attached_children_) {
      if (IsOfClass(*top, cls)) return top.get();
    }
  }
  throw InternalError(
      std::format(
          "Scope::EnclosingInstance: '{}' stands in no instance of the class "
          "its name was resolved against",
          HierarchicalPath().CStr()));
}

auto Scope::InitializationSeeds() -> InitializationRng& {
  return initialization_seeds_;
}

}  // namespace lyra::runtime
