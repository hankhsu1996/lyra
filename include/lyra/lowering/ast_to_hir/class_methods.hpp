#pragma once

#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/method_id.hpp"

namespace slang::ast {
class ClassType;
class ConstraintBlockSymbol;
class CovergroupType;
class FormalArgumentSymbol;
class MethodPrototypeSymbol;
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Every member of a class that holds a position among its methods, in the
// order the class places them: the methods it defines, then its pure virtual
// prototypes (LRM 8.21), then the function evaluating each formal's default
// (LRM 13.5.3), then its constraint blocks (LRM 18.5), then the method making
// each covergroup it embeds (LRM 19.4.1). The constructor holds none (LRM
// 8.7) and is kept beside them for the defaults it declares. The order is a
// function of the class's declaration alone, so the unit declaring the class
// and every unit naming one of its methods count the same positions without
// reading each other.
struct ClassMethods {
  std::vector<const slang::ast::SubroutineSymbol*> defined;
  std::vector<const slang::ast::MethodPrototypeSymbol*> pure_prototypes;
  const slang::ast::SubroutineSymbol* constructor = nullptr;
  std::vector<std::pair<
      const slang::ast::SubroutineSymbol*,
      const slang::ast::FormalArgumentSymbol*>>
      defaulted_formals;
  std::vector<const slang::ast::ConstraintBlockSymbol*> constraint_blocks;
  std::vector<const slang::ast::CovergroupType*> covergroups;

  // How many positions the class's methods take.
  [[nodiscard]] auto size() const -> std::size_t {
    return defined.size() + pure_prototypes.size() + defaulted_formals.size() +
           constraint_blocks.size() + covergroups.size();
  }

  // The position of `method`, a method the class defines or a pure prototype's
  // subroutine.
  [[nodiscard]] auto PositionOf(
      const slang::ast::SubroutineSymbol& method) const -> hir::MethodId;
  // The position of the function evaluating `formal`'s default.
  [[nodiscard]] auto PositionOf(
      const slang::ast::FormalArgumentSymbol& formal) const -> hir::MethodId;
  [[nodiscard]] auto PositionOf(
      const slang::ast::ConstraintBlockSymbol& block) const -> hir::MethodId;
  // At the position of each formal of `method` that has a default, the method
  // evaluating it.
  [[nodiscard]] auto DefaultsOf(const slang::ast::SubroutineSymbol& method)
      const -> std::vector<std::optional<hir::MethodId>>;
  // The position of the method making `covergroup`.
  [[nodiscard]] auto PositionOf(
      const slang::ast::CovergroupType& covergroup) const -> hir::MethodId;
};

auto ClassMethodsOf(const slang::ast::ClassType& cls) -> ClassMethods;

}  // namespace lyra::lowering::ast_to_hir
