#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"

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

// BuiltinCallInfo for builtin method calls (e.g., arr.size(), q.pop_back()).
// - result_type: Required for kNewArray (element type can't be inferred)
// - receiver: For pop methods that both return a value and mutate the queue
struct BuiltinCallInfo {
  BuiltinMethod method;
  TypeId result_type;
  std::optional<PlaceId> receiver;
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
