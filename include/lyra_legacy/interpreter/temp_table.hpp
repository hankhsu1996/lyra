#pragma once

#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

/// Vector-based temp table indexed by per-unit TempId.
/// Size is fixed at construction. InternalError on bounds/uninit access.
class TempTable {
 public:
  /// Construct with count uninitialized slots.
  /// Size is immutable after construction.
  explicit TempTable(size_t count) : values_(count, RuntimeValue::Uninit()) {
  }

  void Write(const lir::TempRef& temp, RuntimeValue value) {
    if (temp.id >= values_.size()) {
      throw common::InternalError("TempTable::Write", "TempId out of range");
    }
    values_[temp.id] = std::move(value);
  }

  [[nodiscard]] auto Read(const lir::TempRef& temp) const
      -> const RuntimeValue& {
    if (temp.id >= values_.size()) {
      throw common::InternalError("TempTable::Read", "TempId out of range");
    }
    if (values_[temp.id].IsUninit()) {
      throw common::InternalError(
          "TempTable::Read", "Read of uninitialized temp");
    }
    return values_[temp.id];
  }

 private:
  std::vector<RuntimeValue> values_;
};

}  // namespace lyra::interpreter
