#pragma once

// What each instance's objects are built as, once the classes are known.
//
// An instance is built as its key worked out again in a naming of its own:
// another instance by what that one is built as, and a block by the class it
// is inside its unit. So it differs from the specialization only where an
// instance or a block is named, and a name landing outside the instance is
// written by the class of each block on its way and the constant it reads
// there. Two specializations built as one thing lower to the same code; a
// child's name leaving the parent is part of the parent's key and one landing
// inside it names the parent by what it is built as, so their children are
// built as one thing too.
//
// It is what a construction names a child by and what a value one unit reads
// of another is found under. Each answer is a function of the elaborated
// design and the classes alone, worked out when first asked and kept.

#include <string>

#include "lyra/lowering/ast_to_hir/unit_grouping.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/support/def_path.hpp"

namespace slang::ast {
class GenerateBlockSymbol;
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class BuiltAsQuery {
 public:
  explicit BuiltAsQuery(const ClassNames& classes)
      : naming_(classes, table_), table_(naming_) {
  }
  BuiltAsQuery(const BuiltAsQuery&) = delete;
  auto operator=(const BuiltAsQuery&) -> BuiltAsQuery& = delete;
  BuiltAsQuery(BuiltAsQuery&&) = delete;
  auto operator=(BuiltAsQuery&&) -> BuiltAsQuery& = delete;
  ~BuiltAsQuery() = default;

  // What the objects of `inst` are built as.
  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string;

 private:
  // The naming the answers are worked out in.
  class Naming final : public UnitNamingPolicy {
   public:
    Naming(const ClassNames& classes, KeyTable& table)
        : UnitNamingPolicy(classes.Keys(), &table),
          classes_(&classes),
          table_(&table) {
    }

    [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
        -> std::string override;
    [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
        const -> support::GenerateBlockStep override;

   private:
    const ClassNames* classes_;
    KeyTable* table_;
  };

  Naming naming_;
  mutable KeyTable table_;
};

}  // namespace lyra::lowering::ast_to_hir
