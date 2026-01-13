#pragma once

#include <optional>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

using SymbolId = common::SymbolId;

// Tracks a variable modification (symbol alone identifies the variable in flat
// storage)
struct VariableChangeRecord {
  SymbolId symbol;
};

struct NbaAction {
  SymbolId symbol;
  RuntimeValue value;
  std::optional<size_t> array_index;  // For element writes: arr[index] <= value
};

struct PostponedAction {
  std::function<void()> action;
};

class ProcessEffect {
 public:
  ProcessEffect() = default;

  // Core effect recording methods
  void RecordVariableModification(SymbolId symbol) {
    modified_variables_.push_back({.symbol = symbol});
  }

  void RecordNbaAction(NbaAction action) {
    nba_actions_.emplace_back(std::move(action));
  }

  void RecordPostponedAction(PostponedAction action) {
    postponed_actions_.emplace_back(std::move(action));
  }

  // Accessors for simulation runner
  [[nodiscard]] auto GetModifiedVariables() const
      -> const std::vector<VariableChangeRecord>& {
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
  std::vector<VariableChangeRecord> modified_variables_;
  std::vector<NbaAction> nba_actions_;
  std::vector<PostponedAction> postponed_actions_;
};

}  // namespace lyra::interpreter
