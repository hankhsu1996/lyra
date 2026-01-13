#pragma once

#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

using SymbolId = common::SymbolId;

class ModuleVariableTable {
 public:
  void Write(SymbolId symbol, const RuntimeValue &value);
  auto Read(SymbolId symbol) const -> RuntimeValue;
  auto ReadPrevious(SymbolId symbol) const -> RuntimeValue;
  void UpdatePrevious(SymbolId symbol, const RuntimeValue &value);
  auto Exists(SymbolId symbol) const -> bool;
  void CreateVariable(SymbolId symbol, RuntimeValue initial_value);
  void InitializeVariable(const common::Variable &variable);

 private:
  std::unordered_map<SymbolId, RuntimeValue> variables_;
  std::unordered_map<SymbolId, RuntimeValue> previous_variables_;
};

class ProcessVariableTable {
 public:
  void Write(SymbolId symbol, const RuntimeValue &value);
  auto Read(SymbolId symbol) const -> RuntimeValue;
  auto Exists(SymbolId symbol) const -> bool;
  void CreateVariable(SymbolId symbol, RuntimeValue initial_value);
  void InitializeVariable(const common::Variable &variable);

 private:
  std::unordered_map<SymbolId, RuntimeValue> variables_;
};

}  // namespace lyra::interpreter
