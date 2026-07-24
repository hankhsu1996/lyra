#pragma once

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;
class Scope;

// Brackets a `context` DPI import's foreign call (LRM 35.5.3) with the scope of
// the import's declaration on the calling process's DPI scope chain. Emitted as
// the first local of the import's marshaling body, it pushes the scope on
// construction and pops it when that body's scope exits -- on a normal return
// or on an unwind, because it is an ordinary scoped local whose destructor the
// C++ (and any future) backend runs at scope end. The chain lives on the
// process, so the push survives a suspension of a time-consuming context import
// and never leaks into another process's chain.
class DpiScopeGuard {
 public:
  DpiScopeGuard(RuntimeEffects& effects, Scope* decl_scope);
  ~DpiScopeGuard();
  DpiScopeGuard(const DpiScopeGuard&) = delete;
  auto operator=(const DpiScopeGuard&) -> DpiScopeGuard& = delete;
  DpiScopeGuard(DpiScopeGuard&&) = delete;
  auto operator=(DpiScopeGuard&&) -> DpiScopeGuard& = delete;

 private:
  RuntimeProcess* process_;
};

}  // namespace lyra::runtime
