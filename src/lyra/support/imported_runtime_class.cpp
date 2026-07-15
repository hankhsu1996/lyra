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
  }
  throw InternalError("ImportedRuntimeMethodSymbol: unknown method");
}

auto ImportedRuntimeMethodIsStatic(ImportedRuntimeMethod method) -> bool {
  switch (method) {
    case ImportedRuntimeMethod::kProcessSelf:
      return true;
    case ImportedRuntimeMethod::kProcessStatus:
      return false;
  }
  throw InternalError("ImportedRuntimeMethodIsStatic: unknown method");
}

}  // namespace lyra::support
