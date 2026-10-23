#include "lyra/support/runtime_class.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto RuntimeClassName(RuntimeClass klass) -> std::string_view {
  switch (klass) {
    case RuntimeClass::kScope:
      return "Scope";
    case RuntimeClass::kObject:
      return "Object";
    case RuntimeClass::kClassObject:
      return "ClassObject";
    case RuntimeClass::kProcess:
      return "Process";
    case RuntimeClass::kSynchronizationObject:
      return "SynchronizationObject";
    case RuntimeClass::kConstraintProblem:
      return "ConstraintProblem";
  }
  throw InternalError("RuntimeClassName: unknown runtime class");
}

auto DeclaringClassOf(LibraryVirtual function) -> RuntimeClass {
  switch (function) {
    case LibraryVirtual::kScopeResolve:
    case LibraryVirtual::kScopeInitialize:
    case LibraryVirtual::kScopeCreateProcesses:
    case LibraryVirtual::kScopeDescribe:
      return RuntimeClass::kScope;
    case LibraryVirtual::kClassRandVariables:
    case LibraryVirtual::kClassConstrain:
    case LibraryVirtual::kClassRandObjects:
    case LibraryVirtual::kClassPreRandomize:
    case LibraryVirtual::kClassPostRandomize:
      return RuntimeClass::kClassObject;
  }
  throw InternalError("DeclaringClassOf: unknown library virtual");
}

auto VirtualsDeclaredBy(RuntimeClass klass) -> std::span<const LibraryVirtual> {
  static constexpr std::array<LibraryVirtual, 4> kScopeVirtuals{
      LibraryVirtual::kScopeResolve, LibraryVirtual::kScopeInitialize,
      LibraryVirtual::kScopeCreateProcesses, LibraryVirtual::kScopeDescribe};
  static constexpr std::array<LibraryVirtual, 5> kClassObjectVirtuals{
      LibraryVirtual::kClassRandVariables, LibraryVirtual::kClassConstrain,
      LibraryVirtual::kClassRandObjects, LibraryVirtual::kClassPreRandomize,
      LibraryVirtual::kClassPostRandomize};
  switch (klass) {
    case RuntimeClass::kScope:
      return kScopeVirtuals;
    case RuntimeClass::kClassObject:
      return kClassObjectVirtuals;
    case RuntimeClass::kObject:
    case RuntimeClass::kProcess:
    case RuntimeClass::kSynchronizationObject:
    case RuntimeClass::kConstraintProblem:
      return {};
  }
  throw InternalError("VirtualsDeclaredBy: unknown runtime class");
}

auto LibraryBaseOf(RuntimeClass klass) -> std::optional<RuntimeClass> {
  switch (klass) {
    case RuntimeClass::kScope:
    case RuntimeClass::kClassObject:
      return RuntimeClass::kObject;
    case RuntimeClass::kObject:
    case RuntimeClass::kProcess:
    case RuntimeClass::kSynchronizationObject:
    case RuntimeClass::kConstraintProblem:
      return std::nullopt;
  }
  throw InternalError("LibraryBaseOf: unknown runtime class");
}

auto OrdinalOf(LibraryVirtual function) -> std::uint32_t {
  const std::span<const LibraryVirtual> declared =
      VirtualsDeclaredBy(DeclaringClassOf(function));
  return static_cast<std::uint32_t>(
      std::ranges::find(declared, function) - declared.begin());
}

auto LibraryVirtualName(LibraryVirtual function) -> std::string_view {
  switch (function) {
    case LibraryVirtual::kScopeResolve:
      return "sv_resolve";
    case LibraryVirtual::kScopeInitialize:
      return "sv_initialize";
    case LibraryVirtual::kScopeCreateProcesses:
      return "sv_create_processes";
    case LibraryVirtual::kScopeDescribe:
      return "sv_describe";
    case LibraryVirtual::kClassRandVariables:
      return "sv_rand_variables";
    case LibraryVirtual::kClassConstrain:
      return "sv_constrain";
    case LibraryVirtual::kClassRandObjects:
      return "sv_rand_objects";
    case LibraryVirtual::kClassPreRandomize:
      return "sv_pre_randomize";
    case LibraryVirtual::kClassPostRandomize:
      return "sv_post_randomize";
  }
  throw InternalError("LibraryVirtualName: unknown library virtual");
}

}  // namespace lyra::support
