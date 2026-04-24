#pragma once

#include <cstdint>

namespace lyra::xir {

struct ExprId {
  uint32_t value = UINT32_MAX;

  auto operator==(const ExprId&) const -> bool = default;
};

struct StmtId {
  uint32_t value = UINT32_MAX;

  auto operator==(const StmtId&) const -> bool = default;
};

struct VariableId {
  uint32_t value = UINT32_MAX;

  auto operator==(const VariableId&) const -> bool = default;
};

constexpr ExprId kInvalidExprId{UINT32_MAX};
constexpr StmtId kInvalidStmtId{UINT32_MAX};
constexpr VariableId kInvalidVariableId{UINT32_MAX};

}  // namespace lyra::xir
