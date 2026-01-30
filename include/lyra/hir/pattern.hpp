#pragma once

#include <variant>

#include "lyra/common/source_span.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

enum class PatternKind {
  kFill,  // '{default: expr}
};

// Fill pattern: replicate expr to fill target under context-typed conversion
// Pattern is shape-directed - the shape comes from the assignment target,
// not from the pattern itself.
struct FillPatternData {
  ExpressionId fill_expr;  // The default value expression
  bool is_bit_fill;        // True if fill value is unbased-unsized (bit-level
                           // fill)
};

using PatternData = std::variant<FillPatternData>;

struct Pattern {
  PatternKind kind;
  SourceSpan span;
  PatternData data;
  // Note: target type is NOT stored here - it comes from the assignment target
};

}  // namespace lyra::hir
