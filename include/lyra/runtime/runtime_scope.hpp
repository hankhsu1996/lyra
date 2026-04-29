#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

class RuntimeScope {
 public:
  RuntimeScope(RuntimeScope* parent, std::string name, RuntimeScopeKind kind);

  [[nodiscard]] auto Parent() const -> RuntimeScope*;
  [[nodiscard]] auto Name() const -> std::string_view;
  [[nodiscard]] auto Kind() const -> RuntimeScopeKind;

  auto AddChildScope(std::string name, RuntimeScopeKind kind) -> RuntimeScope&;
  auto AddProcess(ProcessKind kind, Process process) -> RuntimeProcess&;

  template <typename Fn>
  void ForEachChild(Fn&& fn) {
    auto&& f = std::forward<Fn>(fn);
    for (auto& child : children_) {
      f(*child);
    }
  }
  template <typename Fn>
  void ForEachChild(Fn&& fn) const {
    auto&& f = std::forward<Fn>(fn);
    for (const auto& child : children_) {
      const RuntimeScope& ref = *child;
      f(ref);
    }
  }
  template <typename Fn>
  void ForEachProcess(Fn&& fn) {
    auto&& f = std::forward<Fn>(fn);
    for (auto& process : processes_) {
      f(*process);
    }
  }
  template <typename Fn>
  void ForEachProcess(Fn&& fn) const {
    auto&& f = std::forward<Fn>(fn);
    for (const auto& process : processes_) {
      const RuntimeProcess& ref = *process;
      f(ref);
    }
  }

 private:
  RuntimeScope* parent_ = nullptr;
  std::string name_;
  RuntimeScopeKind kind_;
  std::vector<std::unique_ptr<RuntimeScope>> children_;
  std::vector<std::unique_ptr<RuntimeProcess>> processes_;
};

}  // namespace lyra::runtime
