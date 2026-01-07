#pragma once

#include <memory>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

using SymbolRef = common::SymbolRef;

// Forward declaration
struct InstanceContext;

// Tracks a variable modification with its instance (nullptr = global)
struct ModifiedVariable {
  SymbolRef symbol;
  std::shared_ptr<InstanceContext> instance;  // nullptr for global variables
};

struct NbaAction {
  SymbolRef variable;
  RuntimeValue value;
  std::shared_ptr<InstanceContext> instance;  // nullptr for global variables
};

struct PostponedAction {
  std::function<void()> action;
};

class ProcessEffect {
 public:
  ProcessEffect() = default;

  // Core effect recording methods
  void RecordVariableModification(
      const SymbolRef& symbol,
      const std::shared_ptr<InstanceContext>& instance = nullptr) {
    modified_variables_.push_back({.symbol = symbol, .instance = instance});
  }

  void RecordNbaAction(NbaAction action) {
    nba_actions_.emplace_back(std::move(action));
  }

  void RecordPostponedAction(PostponedAction action) {
    postponed_actions_.emplace_back(std::move(action));
  }

  // Accessors for simulation runner
  [[nodiscard]] auto GetModifiedVariables() const
      -> const std::vector<ModifiedVariable>& {
    return modified_variables_;
  }

  [[nodiscard]] auto GetNbaActions() const -> const std::vector<NbaAction>& {
    return nba_actions_;
  }

  [[nodiscard]] auto GetPostponedActions() const
      -> const std::vector<PostponedAction>& {
    return postponed_actions_;
  }

 private:
  std::vector<ModifiedVariable> modified_variables_;
  std::vector<NbaAction> nba_actions_;
  std::vector<PostponedAction> postponed_actions_;
};

}  // namespace lyra::interpreter
