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

}  // namespace lyra::support
