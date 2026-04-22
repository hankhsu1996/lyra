#pragma once

#include <variant>

#include "lyra/common/source_span.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

enum class PatternKind {
  kFill,  // '{default: expr}
};

struct FillPatternData {
  ExpressionId fill_expr;
};

using PatternData = std::variant<FillPatternData>;

struct Pattern {
  PatternKind kind;
  SourceSpan span;
  PatternData data;
};

}  // namespace lyra::hir
