#include "lyra/support/runtime_class.hpp"

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

auto IsObjectOfTheRoot(RuntimeClass klass) -> bool {
  switch (klass) {
    case RuntimeClass::kScope:
    case RuntimeClass::kObject:
    case RuntimeClass::kClassObject:
    case RuntimeClass::kSynchronizationObject:
    case RuntimeClass::kConstraintProblem:
      return true;
    case RuntimeClass::kProcess:
      return false;
  }
  throw InternalError("IsObjectOfTheRoot: unknown runtime class");
}

auto LibraryCallbackName(LibraryCallback callback) -> std::string_view {
  switch (callback) {
    case LibraryCallback::kScopeResolve:
      return "sv_resolve";
    case LibraryCallback::kScopeInitialize:
      return "sv_initialize";
    case LibraryCallback::kScopeCreateProcesses:
      return "sv_create_processes";
    case LibraryCallback::kScopeDescribe:
      return "sv_describe";
    case LibraryCallback::kClassRandVariables:
      return "sv_rand_variables";
    case LibraryCallback::kClassConstrain:
      return "sv_constrain";
    case LibraryCallback::kClassRandObjects:
      return "sv_rand_objects";
    case LibraryCallback::kClassPreRandomize:
      return "sv_pre_randomize";
    case LibraryCallback::kClassPostRandomize:
      return "sv_post_randomize";
  }
  throw InternalError("LibraryCallbackName: unknown library callback");
}

}  // namespace lyra::support
