#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir {

// Per-kind info structs - each Rvalue kind has its own info type
// The kind is implicit in which variant alternative is active

struct UnaryRvalueInfo {
  UnaryOp op;
};

struct BinaryRvalueInfo {
  BinaryOp op;
};

struct CastRvalueInfo {
  TypeId source_type;
  TypeId target_type;
};

struct SystemCallRvalueInfo {
  int opcode;  // System call opcode
};

struct UserCallRvalueInfo {
  FunctionId callee;  // mir::FunctionId - MIR is self-contained
};

struct AggregateRvalueInfo {
  TypeId result_type;  // The aggregate type being constructed (struct or array)
};

// BuiltinCallRvalueInfo for builtin method calls (e.g., arr.size(), q.pop_back()).
// - result_type: Required for kNewArray (element type can't be inferred)
// - receiver: For pop methods that both return a value and mutate the queue
struct BuiltinCallRvalueInfo {
  BuiltinMethod method;
  TypeId result_type;
  std::optional<PlaceId> receiver;
};

// Variant of all info types - determines Rvalue kind implicitly
using RvalueInfo = std::variant<
    UnaryRvalueInfo, BinaryRvalueInfo, CastRvalueInfo, SystemCallRvalueInfo,
    UserCallRvalueInfo, AggregateRvalueInfo, BuiltinCallRvalueInfo>;

struct Rvalue {
  std::vector<Operand> operands;
  RvalueInfo info;
};

}  // namespace lyra::mir
