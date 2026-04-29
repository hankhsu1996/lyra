#include "lyra/runtime/runtime_scope.hpp"

#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

RuntimeScope::RuntimeScope(
    RuntimeScope* parent, std::string name, RuntimeScopeKind kind)
    : parent_(parent), name_(std::move(name)), kind_(kind) {
}

auto RuntimeScope::Parent() const -> RuntimeScope* {
  return parent_;
}

auto RuntimeScope::Name() const -> std::string_view {
  return name_;
}

auto RuntimeScope::Kind() const -> RuntimeScopeKind {
  return kind_;
}

auto RuntimeScope::AddChildScope(std::string name, RuntimeScopeKind kind)
    -> RuntimeScope& {
  children_.push_back(
      std::make_unique<RuntimeScope>(this, std::move(name), kind));
  return *children_.back();
}

auto RuntimeScope::AddProcess(ProcessKind kind, Process process)
    -> RuntimeProcess& {
  processes_.push_back(
      std::make_unique<RuntimeProcess>(*this, kind, std::move(process)));
  return *processes_.back();
}

}  // namespace lyra::runtime
