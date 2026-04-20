#pragma once

#include <string>

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"

namespace lyra::xir {

// Variable represents an object-owned value-like variable in the compilation
// unit. VariableId is a dense index into CompilationUnit::variables.
//
// This is a first-slice, value-like variable-only prototype path. Only
// integral types <= 32 bits are accepted (projectable to int32_t in C++).
// `Variable` is intentionally narrower than the long-term compilation-unit
// declaration model. When events or non-value-like objects are added later,
// a more abstract concept will be introduced above this.
struct Variable {
  std::string name;
  TypeId type;
  SourceSpan span;
};

}  // namespace lyra::xir
