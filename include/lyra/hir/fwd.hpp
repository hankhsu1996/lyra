#pragma once

#include <cstdint>
#include <utility>

namespace lyra::hir {

struct ExpressionId {
  uint32_t value = UINT32_MAX;

  auto operator==(const ExpressionId&) const -> bool = default;
  auto operator<=>(const ExpressionId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ExpressionId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ExpressionId kInvalidExpressionId{UINT32_MAX};

struct StatementId {
  uint32_t value = UINT32_MAX;

  auto operator==(const StatementId&) const -> bool = default;
  auto operator<=>(const StatementId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, StatementId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr StatementId kInvalidStatementId{UINT32_MAX};

struct ProcessId {
  uint32_t value = UINT32_MAX;

  auto operator==(const ProcessId&) const -> bool = default;
  auto operator<=>(const ProcessId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ProcessId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ProcessId kInvalidProcessId{UINT32_MAX};

struct FunctionId {
  uint32_t value = UINT32_MAX;

  auto operator==(const FunctionId&) const -> bool = default;
  auto operator<=>(const FunctionId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, FunctionId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr FunctionId kInvalidFunctionId{UINT32_MAX};

struct TaskId {
  uint32_t value = UINT32_MAX;

  auto operator==(const TaskId&) const -> bool = default;
  auto operator<=>(const TaskId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, TaskId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr TaskId kInvalidTaskId{UINT32_MAX};

struct PatternId {
  uint32_t value = UINT32_MAX;

  auto operator==(const PatternId&) const -> bool = default;
  auto operator<=>(const PatternId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, PatternId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr PatternId kInvalidPatternId{UINT32_MAX};

struct Expression;
struct Statement;
struct Process;
struct Function;
struct Task;
struct Pattern;

}  // namespace lyra::hir
