#pragma once

#include <cstddef>
#include <string>
#include <vector>

#include "lyra/hir/covergroup_ref.hpp"

namespace slang::ast {
class CoverageBinSymbol;
class CoverCrossSymbol;
class CovergroupBodySymbol;
class CovergroupType;
class CoverpointSymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The coverpoints and the crosses of one covergroup, each in the order the
// source declares them (LRM 19.5, 19.6), a coverpoint a cross declares for a
// variable among them. The order is a function of the covergroup's declaration
// alone, so the unit declaring it and every unit naming one of its items count
// the same places without reading each other.
struct CoverageItems {
  std::vector<const slang::ast::CoverpointSymbol*> coverpoints;
  std::vector<const slang::ast::CoverCrossSymbol*> crosses;

  [[nodiscard]] auto PositionOf(const slang::ast::CoverpointSymbol& point) const
      -> hir::CoverpointId;
  [[nodiscard]] auto PositionOf(const slang::ast::CoverCrossSymbol& cross) const
      -> hir::CrossId;
};

auto CoverageItemsOf(const slang::ast::CovergroupBodySymbol& body)
    -> CoverageItems;

// The name each item `items` holds is reported and written to the database by
// (LRM 19.5, 19.9), its covergroup extending ones holding `unnamed_above`
// items the source gave no name: its label, or the variable a coverpoint
// written with none is over; otherwise a name holding a space, which no
// identifier does, escaped ones included (LRM 5.6.1), numbered along the
// lineage so no two items of one type share one.
struct ReportNames {
  std::vector<std::string> coverpoints;
  std::vector<std::string> crosses;
  std::size_t unnamed = 0;
};
auto ReportNamesOf(const CoverageItems& items, std::size_t unnamed_above)
    -> ReportNames;

// The place of `bin` among the bin constructs of its coverpoint, in the order
// the source declares them.
auto BinConstructOf(const slang::ast::CoverageBinSymbol& bin)
    -> hir::BinConstructId;

// The covergroup whose body declares `item`, a coverpoint or a cross.
auto CovergroupDeclaring(const slang::ast::Symbol& item)
    -> const slang::ast::CovergroupType&;

}  // namespace lyra::lowering::ast_to_hir
