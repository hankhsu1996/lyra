#pragma once

#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

enum class RvalueKind {
  kUnary,
  kBinary,
  kCast,
  kCall,
};

struct CastInfo {
  TypeId source_type;
  TypeId target_type;
};

using RvalueInfo = std::variant<std::monostate, CastInfo>;

struct Rvalue {
  RvalueKind kind;
  int op;  // opcode / operation kind
  std::vector<Operand> operands;
  RvalueInfo info;
};

}  // namespace lyra::mir
