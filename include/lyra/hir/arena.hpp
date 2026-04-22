#pragma once

#include <vector>

#include "lyra/hir/declaration.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/generate.hpp"
#include "lyra/hir/pattern.hpp"
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
  void ReservePatterns(size_t capacity) {
    patterns_.reserve(capacity);
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

  auto AddPattern(Pattern pattern) -> PatternId {
    PatternId id{static_cast<uint32_t>(patterns_.size())};
    patterns_.push_back(std::move(pattern));
    return id;
  }

  auto AddVariable(Variable var) -> VariableId {
    VariableId id{static_cast<uint32_t>(variables_.size())};
    variables_.push_back(std::move(var));
    return id;
  }

  auto AddNet(Net net) -> NetId {
    NetId id{static_cast<uint32_t>(nets_.size())};
    nets_.push_back(std::move(net));
    return id;
  }

  auto AddParameter(Parameter param) -> ParameterId {
    ParameterId id{static_cast<uint32_t>(parameters_.size())};
    parameters_.push_back(std::move(param));
    return id;
  }

  auto AddInstanceMember(InstanceMember member) -> InstanceMemberId {
    InstanceMemberId id{static_cast<uint32_t>(instance_members_.size())};
    instance_members_.push_back(std::move(member));
    return id;
  }

  auto AddGenerateRegion(GenerateRegion region) -> GenerateRegionId {
    GenerateRegionId id{static_cast<uint32_t>(generate_regions_.size())};
    generate_regions_.push_back(std::move(region));
    return id;
  }

  auto AddGenerateConstruct(GenerateConstruct construct)
      -> GenerateConstructId {
    GenerateConstructId id{static_cast<uint32_t>(generate_constructs_.size())};
    generate_constructs_.push_back(std::move(construct));
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

  [[nodiscard]] auto operator[](PatternId id) const -> const Pattern& {
    return patterns_[id.value];
  }

  [[nodiscard]] auto operator[](VariableId id) const -> const Variable& {
    return variables_[id.value];
  }

  [[nodiscard]] auto operator[](NetId id) const -> const Net& {
    return nets_[id.value];
  }

  [[nodiscard]] auto operator[](ParameterId id) const -> const Parameter& {
    return parameters_[id.value];
  }

  [[nodiscard]] auto operator[](InstanceMemberId id) const
      -> const InstanceMember& {
    return instance_members_[id.value];
  }

  [[nodiscard]] auto operator[](GenerateRegionId id) const
      -> const GenerateRegion& {
    return generate_regions_[id.value];
  }

  [[nodiscard]] auto operator[](GenerateRegionId id) -> GenerateRegion& {
    return generate_regions_[id.value];
  }

  [[nodiscard]] auto operator[](GenerateConstructId id) const
      -> const GenerateConstruct& {
    return generate_constructs_[id.value];
  }

  [[nodiscard]] auto operator[](GenerateConstructId id) -> GenerateConstruct& {
    return generate_constructs_[id.value];
  }

 private:
  std::vector<Expression> expressions_;
  std::vector<Statement> statements_;
  std::vector<Process> processes_;
  std::vector<Function> functions_;
  std::vector<Task> tasks_;
  std::vector<Pattern> patterns_;
  std::vector<Variable> variables_;
  std::vector<Net> nets_;
  std::vector<Parameter> parameters_;
  std::vector<InstanceMember> instance_members_;
  std::vector<GenerateRegion> generate_regions_;
  std::vector<GenerateConstruct> generate_constructs_;
};

}  // namespace lyra::hir
