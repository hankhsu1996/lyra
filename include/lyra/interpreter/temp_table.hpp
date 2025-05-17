#pragma once

#include <unordered_map>

#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

class TempTable {
 public:
  void Write(const lir::TempRef& temp, const RuntimeValue& value);
  auto Read(const lir::TempRef& temp) const -> RuntimeValue;

 private:
  std::unordered_map<lir::TempRef, RuntimeValue> registers_;
};

}  // namespace lyra::interpreter
