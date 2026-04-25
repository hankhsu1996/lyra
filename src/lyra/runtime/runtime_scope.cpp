#include "lyra/runtime/runtime_scope.hpp"

#include <span>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

RuntimeScope::RuntimeScope(
    RuntimeScope* parent, std::string name, RuntimeScopeKind kind)
    : parent_(parent), name_(std::move(name)), kind_(kind) {
}

auto RuntimeScope::Parent() -> RuntimeScope* {
  return parent_;
}

auto RuntimeScope::Name() const -> std::string_view {
  return name_;
}

auto RuntimeScope::Kind() const -> RuntimeScopeKind {
  return kind_;
}

void RuntimeScope::AddChild(RuntimeScope& child) {
  children_.push_back(&child);
}

auto RuntimeScope::Children() const -> std::span<RuntimeScope* const> {
  return children_;
}

}  // namespace lyra::runtime
