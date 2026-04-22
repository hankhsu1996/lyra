#pragma once

#include <optional>
#include <vector>

#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

enum class ProcessKind {
  kInitial,
  kAlways,
  kAlwaysComb,
  kAlwaysFf,
  kAlwaysLatch,
  kFinal,
};

struct Process {
  ProcessKind kind = ProcessKind::kInitial;
  SourceSpan span;
  StatementId body;
};

// HIR representation of a subroutine parameter.
struct FunctionParam {
  SymbolId symbol;
  ParameterDirection direction = ParameterDirection::kInput;
};

struct Function {
  SymbolId symbol;
  SourceSpan span;
  TypeId return_type;
  std::vector<FunctionParam> parameters;
  StatementId body;
  // Implicit local variable for return-by-name assignment (absent for void)
  std::optional<SymbolId> return_var;
};

struct Task {
  SymbolId symbol;
  SourceSpan span;
  std::vector<FunctionParam> parameters;
  StatementId body;
};

}  // namespace lyra::hir
