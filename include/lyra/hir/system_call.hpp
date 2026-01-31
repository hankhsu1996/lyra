#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/runtime_query_kind.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// A single format operation within a display/write call.
// Either a literal string or a formatted value.
struct FormatOp {
  FormatKind kind;
  std::optional<ExpressionId> value;  // nullopt for kLiteral
  std::string literal;                // only valid when kind == kLiteral
  FormatModifiers mods;               // width, precision, flags
  int8_t module_timeunit_power =
      -9;  // For kTime: unit of value (from scope's timescale)

  auto operator==(const FormatOp&) const -> bool = default;
};

struct DisplaySystemCallData {
  PrintKind print_kind;
  std::vector<FormatOp> ops;
  std::optional<ExpressionId> descriptor;  // nullopt = stdout
  bool is_strobe = false;  // true for $strobe/$strobeb/$strobeo/$strobeh

  auto operator==(const DisplaySystemCallData&) const -> bool = default;
};

struct SeveritySystemCallData {
  Severity level;
  std::vector<FormatOp> ops;

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

struct FflushData {
  std::optional<ExpressionId> descriptor;  // nullopt = flush all

  auto operator==(const FflushData&) const -> bool = default;
};

struct RuntimeQueryData {
  RuntimeQueryKind kind;

  auto operator==(const RuntimeQueryData&) const -> bool = default;
};

// $timeformat(units, precision, suffix, min_width)
// All arguments are compile-time constants (validated in AST->HIR).
struct TimeFormatData {
  int8_t units = 127;  // 127 = sentinel for "use global precision"
  int precision = 0;   // decimal digits
  std::string suffix;  // appended string
  int min_width = 20;  // minimum field width

  auto operator==(const TimeFormatData&) const -> bool = default;
};

// $monitor/$monitorb/$monitoro/$monitorh - persistent change-triggered display
struct MonitorSystemCallData {
  PrintKind print_kind;
  std::vector<FormatOp> ops;
  std::optional<ExpressionId> descriptor;  // nullopt = stdout

  auto operator==(const MonitorSystemCallData&) const -> bool = default;
};

// $monitoron/$monitoroff - control monitor enabled state
struct MonitorControlData {
  bool enable;  // true=$monitoron, false=$monitoroff

  auto operator==(const MonitorControlData&) const -> bool = default;
};

// $random, $urandom - random number generation
struct RandomData {
  RandomKind kind;

  auto operator==(const RandomData&) const -> bool = default;
};

using SystemCallExpressionData = std::variant<
    DisplaySystemCallData, SeveritySystemCallData, SFormatSystemCallData,
    TestPlusargsData, ValuePlusargsData, MemIOData, FopenData, FcloseData,
    FflushData, RuntimeQueryData, TimeFormatData, MonitorSystemCallData,
    MonitorControlData, RandomData>;

}  // namespace lyra::hir
