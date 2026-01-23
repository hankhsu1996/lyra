#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// A single format operation within a display/write call.
// Either a literal string or a formatted value.
struct FormatOp {
  FormatKind kind;
  std::optional<ExpressionId> value;  // nullopt for kLiteral
  std::string literal;                // only valid when kind == kLiteral
  FormatModifiers mods;               // width, precision, flags

  auto operator==(const FormatOp&) const -> bool = default;
};

struct DisplaySystemCallData {
  PrintKind print_kind;
  std::vector<FormatOp> ops;

  auto operator==(const DisplaySystemCallData&) const -> bool = default;
};

struct SeveritySystemCallData {
  Severity level;
  std::vector<ExpressionId> args;

  auto operator==(const SeveritySystemCallData&) const -> bool = default;
};

struct SFormatSystemCallData {
  std::vector<FormatOp> ops;  // Compile-time format ops (empty = runtime path)
  std::vector<ExpressionId> args;      // Runtime args (used when ops is empty)
  std::optional<ExpressionId> output;  // Output lvalue (for $sformat/$swrite*)
  FormatKind default_format = FormatKind::kDecimal;

  auto operator==(const SFormatSystemCallData&) const -> bool = default;
};

struct TestPlusargsData {
  ExpressionId query;

  auto operator==(const TestPlusargsData&) const -> bool = default;
};

struct ValuePlusargsData {
  ExpressionId format;
  ExpressionId output;  // The lvalue target

  auto operator==(const ValuePlusargsData&) const -> bool = default;
};

struct MemIOData {
  bool is_read = false;  // readmem vs writemem
  bool is_hex = false;   // hex vs binary
  ExpressionId filename;
  ExpressionId target;  // The memory array lvalue
  std::optional<ExpressionId> start_addr;
  std::optional<ExpressionId> end_addr;

  auto operator==(const MemIOData&) const -> bool = default;
};

struct FopenData {
  ExpressionId filename;
  std::optional<ExpressionId> mode;  // nullopt = MCD mode, present = FD mode

  auto operator==(const FopenData&) const -> bool = default;
};

struct FcloseData {
  ExpressionId descriptor;

  auto operator==(const FcloseData&) const -> bool = default;
};

using SystemCallExpressionData = std::variant<
    DisplaySystemCallData, SeveritySystemCallData, SFormatSystemCallData,
    TestPlusargsData, ValuePlusargsData, MemIOData, FopenData, FcloseData>;

}  // namespace lyra::hir
