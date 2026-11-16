#include "lyra/support/scope_kind.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ExecutionOf(DefinitionKind kind) -> ScopeExecution {
  switch (kind) {
    case DefinitionKind::kProgram:
      return ScopeExecution::kProgram;
    case DefinitionKind::kModule:
    case DefinitionKind::kInterface:
    case DefinitionKind::kPackage:
    case DefinitionKind::kCompilationUnit:
    case DefinitionKind::kDesignRoot:
      return ScopeExecution::kDesign;
  }
  throw InternalError("ExecutionOf: unknown kind");
}

auto IsCompilationUnit(DefinitionKind kind) -> bool {
  switch (kind) {
    case DefinitionKind::kCompilationUnit:
      return true;
    case DefinitionKind::kModule:
    case DefinitionKind::kInterface:
    case DefinitionKind::kProgram:
    case DefinitionKind::kPackage:
    case DefinitionKind::kDesignRoot:
      return false;
  }
  throw InternalError("IsCompilationUnit: unknown kind");
}

}  // namespace lyra::support
