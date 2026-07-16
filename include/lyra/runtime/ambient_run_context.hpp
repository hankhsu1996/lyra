#pragma once

namespace lyra::runtime {

class Scope;

// The design context in effect for the duration of a running simulation. A
// foreign C function that calls back an exported SV subroutine (LRM 35.5)
// reaches the wrapper with plain C arguments and no design pointer, so the
// wrapper has nothing to reach the design from unless the runtime anchors it
// ambiently. This holds the design root for that recovery.
//
// It is an RAII stack guard over a thread-local slot: installed once around the
// simulation run at the single run entry, and torn down when the run returns.
// Nested foreign entries (an import called from inside an export) push and pop,
// so the innermost active run is always current. The context is valid only
// while a simulation is running on the current thread; a callback on a
// different thread must install its own.
class AmbientRunContext {
 public:
  explicit AmbientRunContext(Scope* root);
  ~AmbientRunContext();
  AmbientRunContext(const AmbientRunContext&) = delete;
  auto operator=(const AmbientRunContext&) -> AmbientRunContext& = delete;
  AmbientRunContext(AmbientRunContext&&) = delete;
  auto operator=(AmbientRunContext&&) -> AmbientRunContext& = delete;

  [[nodiscard]] auto Root() const -> Scope* {
    return root_;
  }

  static auto Current() -> AmbientRunContext&;

 private:
  Scope* root_;
  AmbientRunContext* previous_;
};

// Resolves the instance an exported subroutine runs against: the child of the
// design root registered under `instance_name`. The exported subroutine is a
// method of that instance, so the wrapper needs the instance as its receiver.
auto ResolveExportInstance(const char* instance_name) -> Scope*;

}  // namespace lyra::runtime
