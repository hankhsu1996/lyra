#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

struct NbaAction {
  std::string variable;
  RuntimeValue value;
};

struct PostponedAction {
  std::function<void()> action;
};

class ProcessEffect {
 public:
  ProcessEffect() = default;

  // Core effect recording methods
  void RecordVariableModification(std::string_view variable) {
    modified_variables_.emplace_back(variable);
  }

  void RecordNbaAction(NbaAction action) {
    nba_actions_.emplace_back(std::move(action));
  }

  void RecordPostponedAction(PostponedAction action) {
    postponed_actions_.emplace_back(std::move(action));
  }

  // Accessors for simulation runner
  [[nodiscard]] auto GetModifiedVariables() const
      -> const std::vector<std::string>& {
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
  std::vector<std::string> modified_variables_;
  std::vector<NbaAction> nba_actions_;
  std::vector<PostponedAction> postponed_actions_;
};

}  // namespace lyra::interpreter
