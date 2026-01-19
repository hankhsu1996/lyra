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

struct SelectRvalueInfo {
  // operands[0] = condition (1-bit 2-state bool)
  // operands[1] = true_value
  // operands[2] = false_value
};

// Variant of all info types - determines Rvalue kind implicitly
using RvalueInfo = std::variant<
    UnaryRvalueInfo, BinaryRvalueInfo, CastRvalueInfo, SystemCallRvalueInfo,
    UserCallRvalueInfo, AggregateRvalueInfo, BuiltinCallRvalueInfo,
    SelectRvalueInfo>;

struct Rvalue {
  std::vector<Operand> operands;
  RvalueInfo info;
};

// Helper to get Rvalue kind name from info variant
inline auto GetRvalueKind(const RvalueInfo& info) -> const char* {
  return std::visit(
      [](const auto& i) -> const char* {
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, UnaryRvalueInfo>) {
          return "unary";
        } else if constexpr (std::is_same_v<T, BinaryRvalueInfo>) {
          return "binary";
        } else if constexpr (std::is_same_v<T, CastRvalueInfo>) {
          return "cast";
        } else if constexpr (std::is_same_v<T, SystemCallRvalueInfo>) {
          return "syscall";
        } else if constexpr (std::is_same_v<T, UserCallRvalueInfo>) {
          return "call";
        } else if constexpr (std::is_same_v<T, AggregateRvalueInfo>) {
          return "aggregate";
        } else if constexpr (std::is_same_v<T, BuiltinCallRvalueInfo>) {
          return "builtin";
        } else if constexpr (std::is_same_v<T, SelectRvalueInfo>) {
          return "select";
        } else {
          static_assert(false, "unhandled RvalueInfo kind");
        }
      },
      info);
}

}  // namespace lyra::mir
