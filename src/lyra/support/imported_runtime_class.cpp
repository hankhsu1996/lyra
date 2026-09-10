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

auto RuntimeEntryOf(ImportedRuntimeMethod method) -> RuntimeEntry {
  switch (method) {
    case ImportedRuntimeMethod::kProcessSelf:
      return {
          .name = "process_self",
          .declaration = FreeFunction{"lyra::runtime::ProcessSelf"}};
    case ImportedRuntimeMethod::kProcessStatus:
      return {
          .name = "process_status",
          .declaration = FreeFunction{"lyra::runtime::ProcessStatus"}};
    case ImportedRuntimeMethod::kProcessKill:
      return {
          .name = "process_kill",
          .declaration = FreeFunction{"lyra::runtime::ProcessKill"}};
    case ImportedRuntimeMethod::kProcessAwait:
      return {
          .name = "process_await",
          .declaration = FreeFunction{"lyra::runtime::ProcessAwait"}};
    case ImportedRuntimeMethod::kProcessSuspend:
      return {
          .name = "process_suspend",
          .declaration = FreeFunction{"lyra::runtime::ProcessSuspend"}};
    case ImportedRuntimeMethod::kProcessResume:
      return {
          .name = "process_resume",
          .declaration = FreeFunction{"lyra::runtime::ProcessResume"}};
  }
  throw InternalError("RuntimeEntryOf: unknown imported runtime method");
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
