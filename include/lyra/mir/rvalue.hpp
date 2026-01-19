#pragma once

#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

enum class RvalueKind {
  kUnary,
  kBinary,
  kCast,
  kCall,
  kAggregate,
  kBuiltinCall,
};

struct CastInfo {
  TypeId source_type;
  TypeId target_type;
};

struct AggregateInfo {
  TypeId result_type;  // The aggregate type being constructed (struct or array)
};

struct SystemCallInfo {
  int opcode;  // System call opcode
};

struct UserCallInfo {
  FunctionId callee;  // mir::FunctionId - MIR is self-contained
};

struct BuiltinCallInfo {
  BuiltinMethod method;
  TypeId result_type;  // Required: kNewArray needs element type, can't infer
};

using RvalueInfo = std::variant<
    std::monostate, CastInfo, AggregateInfo, SystemCallInfo, UserCallInfo,
    BuiltinCallInfo>;

struct Rvalue {
  RvalueKind kind;
  int op;  // opcode / operation kind
  std::vector<Operand> operands;
  RvalueInfo info;
};

}  // namespace lyra::mir
