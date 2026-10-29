#include "lyra/lowering/ast_to_hir/built_as.hpp"

#include <string>

namespace lyra::lowering::ast_to_hir {

auto BuiltAsQuery::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  return table_.NameOf(inst);
}

auto BuiltAsQuery::Naming::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  return table_->NameOf(inst);
}

auto BuiltAsQuery::Naming::BlockStepOf(
    const slang::ast::GenerateBlockSymbol& block) const
    -> support::GenerateBlockStep {
  return classes_->BlockStepOf(block);
}

}  // namespace lyra::lowering::ast_to_hir
