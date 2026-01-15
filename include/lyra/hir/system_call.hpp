#pragma once

#include <variant>
#include <vector>

#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

enum class PrintRadix {
  kDecimal,
  kBinary,
  kOctal,
  kHex,
};

enum class FinishKind {
  kFinish,
  kStop,
  kExit,
};

enum class TimeKind {
  kTime,
  kStime,
  kRealtime,
};

struct DisplaySystemCallData {
  PrintRadix radix;
  bool append_newline;
  std::vector<ExpressionId> args;

  auto operator==(const DisplaySystemCallData&) const -> bool = default;
};

struct FinishSystemCallData {
  FinishKind kind;

  auto operator==(const FinishSystemCallData&) const -> bool = default;
};

struct TimeSystemCallData {
  TimeKind kind;

  auto operator==(const TimeSystemCallData&) const -> bool = default;
};

using SystemCallExpressionData = std::variant<
    DisplaySystemCallData, FinishSystemCallData, TimeSystemCallData>;

}  // namespace lyra::hir
