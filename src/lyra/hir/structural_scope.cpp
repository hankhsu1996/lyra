#include "lyra/hir/structural_scope.hpp"

#include <utility>

namespace lyra::hir {

StructuralScope::StructuralScope() = default;
StructuralScope::~StructuralScope() = default;
StructuralScope::StructuralScope(StructuralScope&&) noexcept = default;
auto StructuralScope::operator=(StructuralScope&&) noexcept
    -> StructuralScope& = default;

auto StructuralScope::AddVarDecl(std::string name, TypeId type) -> VarDeclId {
  const VarDeclId id{static_cast<std::uint32_t>(var_decls_.size())};
  var_decls_.push_back(VarDecl{.name = std::move(name), .type = type});
  return id;
}

auto StructuralScope::AppendExpr(Expr expr) -> ExprId {
  const ExprId id{static_cast<std::uint32_t>(exprs_.size())};
  exprs_.push_back(std::move(expr));
  return id;
}

auto StructuralScope::AddProcess(Process process) -> ProcessId {
  const ProcessId id{static_cast<std::uint32_t>(processes_.size())};
  processes_.push_back(std::move(process));
  return id;
}

auto StructuralScope::AddGenerate(Generate generate) -> GenerateId {
  const GenerateId id{static_cast<std::uint32_t>(generates_.size())};
  generates_.push_back(std::move(generate));
  return id;
}

auto StructuralScope::VarDecls() const -> const std::vector<VarDecl>& {
  return var_decls_;
}

auto StructuralScope::Exprs() const -> const std::vector<Expr>& {
  return exprs_;
}

auto StructuralScope::Processes() const -> const std::vector<Process>& {
  return processes_;
}

auto StructuralScope::Generates() const -> const std::vector<Generate>& {
  return generates_;
}

auto StructuralScope::GetVarDecl(VarDeclId id) const -> const VarDecl& {
  return var_decls_.at(id.value);
}

auto StructuralScope::GetExpr(ExprId id) const -> const Expr& {
  return exprs_.at(id.value);
}

auto StructuralScope::GetProcess(ProcessId id) const -> const Process& {
  return processes_.at(id.value);
}

auto StructuralScope::GetGenerate(GenerateId id) const -> const Generate& {
  return generates_.at(id.value);
}

}  // namespace lyra::hir
