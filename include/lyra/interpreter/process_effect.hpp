#pragma once

#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

using SymbolRef = common::SymbolRef;

struct NbaAction {
  SymbolRef variable;
  RuntimeValue value;
};

struct PostponedAction {
  std::function<void()> action;
};

class ProcessEffect {
 public:
  ProcessEffect() = default;

  // Core effect recording methods
  void RecordVariableModification(const SymbolRef& symbol) {
    modified_variables_.emplace_back(symbol);
  }

  void RecordNbaAction(NbaAction action) {
    nba_actions_.emplace_back(std::move(action));
  }

  void RecordPostponedAction(PostponedAction action) {
    postponed_actions_.emplace_back(std::move(action));
  }

  // Accessors for simulation runner
  [[nodiscard]] auto GetModifiedVariables() const
      -> const std::vector<SymbolRef>& {
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
  std::vector<SymbolRef> modified_variables_;
  std::vector<NbaAction> nba_actions_;
  std::vector<PostponedAction> postponed_actions_;
};

}  // namespace lyra::interpreter
