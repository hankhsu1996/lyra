#pragma once

#include <optional>
#include <vector>

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

// Parameter direction for subroutine arguments.
// Maps directly to SystemVerilog argument directions (IEEE 1800-2017 13.5).
enum class ParameterDirection {
  kInput,   // Input only (pass by value semantics)
  kOutput,  // Output only (callee writes, caller receives)
  kInOut,  // Bidirectional (caller passes initialized value, callee may modify)
  kRef,    // Reference (alias to caller's variable) - not yet supported
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
