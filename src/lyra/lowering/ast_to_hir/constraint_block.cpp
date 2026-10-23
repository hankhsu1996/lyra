#include "lyra/lowering/ast_to_hir/constraint_block.hpp"

#include <string_view>

#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/types/Type.h>

namespace lyra::lowering::ast_to_hir {

auto OverriddenConstraintBlock(
    const slang::ast::ClassType& cls, std::string_view name)
    -> const slang::ast::ConstraintBlockSymbol* {
  const slang::ast::Type* base = cls.getBaseClass();
  while (base != nullptr) {
    const auto& base_class =
        base->getCanonicalType().as<slang::ast::ClassType>();
    for (const auto& block :
         base_class.membersOfType<slang::ast::ConstraintBlockSymbol>()) {
      if (block.getParentScope() == &base_class && block.name == name) {
        return &block;
      }
    }
    base = base_class.getBaseClass();
  }
  return nullptr;
}

auto IntroducingConstraintBlock(const slang::ast::ConstraintBlockSymbol& block)
    -> const slang::ast::ConstraintBlockSymbol& {
  const slang::ast::ConstraintBlockSymbol* introducing = &block;
  while (const slang::ast::ConstraintBlockSymbol* overridden =
             OverriddenConstraintBlock(
                 introducing->getParentScope()
                     ->asSymbol()
                     .as<slang::ast::ClassType>(),
                 introducing->name)) {
    introducing = overridden;
  }
  return *introducing;
}

}  // namespace lyra::lowering::ast_to_hir
