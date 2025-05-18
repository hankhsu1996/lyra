#pragma once

#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

using SymbolRef = common::SymbolRef;

class ModuleVariableTable {
 public:
  void Write(const SymbolRef &symbol, const RuntimeValue &value);
  auto Read(const SymbolRef &symbol) const -> RuntimeValue;
  auto ReadFromName(const std::string &name) const -> RuntimeValue;
  auto ReadPrevious(const SymbolRef &symbol) const -> RuntimeValue;
  void UpdatePrevious(const SymbolRef &symbol, const RuntimeValue &value);
  auto Exists(const SymbolRef &symbol) const -> bool;
  void CreateVariable(const SymbolRef &symbol, RuntimeValue initial_value);
  void InitializeVariable(const common::Variable &variable);

 private:
  std::unordered_map<SymbolRef, RuntimeValue> variables_;
  std::unordered_map<SymbolRef, RuntimeValue> previous_variables_;
};

class ProcessVariableTable {
 public:
  void Write(const SymbolRef &symbol, const RuntimeValue &value);
  auto Read(const SymbolRef &symbol) const -> RuntimeValue;
  auto ReadFromName(const std::string &name) const -> RuntimeValue;
  auto Exists(const SymbolRef &symbol) const -> bool;
  void CreateVariable(const SymbolRef &symbol, RuntimeValue initial_value);
  void InitializeVariable(const common::Variable &variable);

 private:
  std::unordered_map<SymbolRef, RuntimeValue> variables_;
};

}  // namespace lyra::interpreter
