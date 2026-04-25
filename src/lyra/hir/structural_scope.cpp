#include "lyra/hir/structural_scope.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::hir {

StructuralScope::StructuralScope() = default;
StructuralScope::~StructuralScope() = default;
StructuralScope::StructuralScope(StructuralScope&&) noexcept = default;
auto StructuralScope::operator=(StructuralScope&&) noexcept
    -> StructuralScope& = default;

auto StructuralScope::AddMemberVar(std::string name, TypeId type)
    -> MemberVarId {
  const MemberVarId id{static_cast<std::uint32_t>(member_vars_.size())};
  member_vars_.push_back(MemberVar{.name = std::move(name), .type = type});
  return id;
}

auto StructuralScope::AddExpr(Expr expr) -> ExprId {
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

auto StructuralScope::MemberVars() const -> const std::vector<MemberVar>& {
  return member_vars_;
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

auto StructuralScope::GetMemberVar(MemberVarId id) const -> const MemberVar& {
  return member_vars_.at(id.value);
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
