#include "lyra/support/imported_runtime_class.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ImportedRuntimeClassName(ImportedRuntimeClass klass) -> std::string_view {
  switch (klass) {
    case ImportedRuntimeClass::kProcess:
      return "process";
  }
  throw InternalError("ImportedRuntimeClassName: unknown imported class");
}

auto ImportedRuntimeMethodSymbol(ImportedRuntimeMethod method)
    -> std::string_view {
  switch (method) {
    case ImportedRuntimeMethod::kProcessSelf:
      return "ProcessSelf";
    case ImportedRuntimeMethod::kProcessStatus:
      return "ProcessStatus";
    case ImportedRuntimeMethod::kProcessKill:
      return "ProcessKill";
    case ImportedRuntimeMethod::kProcessAwait:
      return "ProcessAwait";
    case ImportedRuntimeMethod::kProcessSuspend:
      return "ProcessSuspend";
    case ImportedRuntimeMethod::kProcessResume:
      return "ProcessResume";
  }
  throw InternalError("ImportedRuntimeMethodSymbol: unknown method");
}

auto ImportedRuntimeMethodTakesServices(ImportedRuntimeMethod method) -> bool {
  switch (method) {
    case ImportedRuntimeMethod::kProcessSelf:
    case ImportedRuntimeMethod::kProcessKill:
    case ImportedRuntimeMethod::kProcessAwait:
    case ImportedRuntimeMethod::kProcessSuspend:
    case ImportedRuntimeMethod::kProcessResume:
      return true;
    case ImportedRuntimeMethod::kProcessStatus:
      return false;
  }
  throw InternalError("ImportedRuntimeMethodTakesServices: unknown method");
}

auto ImportedRuntimeMethodSuspends(ImportedRuntimeMethod method) -> bool {
  switch (method) {
    case ImportedRuntimeMethod::kProcessAwait:
      return true;
    case ImportedRuntimeMethod::kProcessSelf:
    case ImportedRuntimeMethod::kProcessStatus:
    case ImportedRuntimeMethod::kProcessKill:
    case ImportedRuntimeMethod::kProcessSuspend:
    case ImportedRuntimeMethod::kProcessResume:
      return false;
  }
  throw InternalError("ImportedRuntimeMethodSuspends: unknown method");
}

}  // namespace lyra::support
