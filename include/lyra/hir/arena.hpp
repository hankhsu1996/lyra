#pragma once

#include <vector>

#include "lyra/hir/expression.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"

namespace lyra::hir {

class Arena final {
 public:
  Arena() = default;
  ~Arena() = default;

  Arena(const Arena&) = delete;
  auto operator=(const Arena&) -> Arena& = delete;

  Arena(Arena&&) = default;
  auto operator=(Arena&&) -> Arena& = default;

  void ReserveExpressions(size_t capacity) {
    expressions_.reserve(capacity);
  }
  void ReserveStatements(size_t capacity) {
    statements_.reserve(capacity);
  }
  void ReserveProcesses(size_t capacity) {
    processes_.reserve(capacity);
  }
  void ReserveFunctions(size_t capacity) {
    functions_.reserve(capacity);
  }
  void ReserveTasks(size_t capacity) {
    tasks_.reserve(capacity);
  }

  auto AddExpression(Expression expr) -> ExpressionId {
    ExpressionId id{static_cast<uint32_t>(expressions_.size())};
    expressions_.push_back(std::move(expr));
    return id;
  }

  auto AddStatement(Statement stmt) -> StatementId {
    StatementId id{static_cast<uint32_t>(statements_.size())};
    statements_.push_back(std::move(stmt));
    return id;
  }

  auto AddProcess(Process proc) -> ProcessId {
    ProcessId id{static_cast<uint32_t>(processes_.size())};
    processes_.push_back(std::move(proc));
    return id;
  }

  auto AddFunction(Function func) -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    functions_.push_back(std::move(func));
    return id;
  }

  auto AddTask(Task task) -> TaskId {
    TaskId id{static_cast<uint32_t>(tasks_.size())};
    tasks_.push_back(std::move(task));
    return id;
  }

  [[nodiscard]] auto operator[](ExpressionId id) const -> const Expression& {
    return expressions_[id.value];
  }

  [[nodiscard]] auto operator[](StatementId id) const -> const Statement& {
    return statements_[id.value];
  }

  [[nodiscard]] auto operator[](ProcessId id) const -> const Process& {
    return processes_[id.value];
  }

  [[nodiscard]] auto operator[](FunctionId id) const -> const Function& {
    return functions_[id.value];
  }

  [[nodiscard]] auto operator[](TaskId id) const -> const Task& {
    return tasks_[id.value];
  }

 private:
  std::vector<Expression> expressions_;
  std::vector<Statement> statements_;
  std::vector<Process> processes_;
  std::vector<Function> functions_;
  std::vector<Task> tasks_;
};

}  // namespace lyra::hir
