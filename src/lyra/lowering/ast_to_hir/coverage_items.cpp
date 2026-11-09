#include "lyra/lowering/ast_to_hir/coverage_items.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>

#include <slang/ast/symbols/CoverSymbols.h>

#include "lyra/base/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

template <typename Symbol>
auto PlaceIn(const std::vector<const Symbol*>& items, const Symbol& item)
    -> std::uint32_t {
  for (std::size_t i = 0; i < items.size(); ++i) {
    if (items[i] == &item) return static_cast<std::uint32_t>(i);
  }
  throw InternalError(
      std::format(
          "CoverageItems::PositionOf: '{}' is no item of the covergroup "
          "declaring it -- please report this as a bug",
          item.name));
}

}  // namespace

auto CoverageItems::PositionOf(const slang::ast::CoverpointSymbol& point) const
    -> hir::CoverpointId {
  return hir::CoverpointId{PlaceIn(coverpoints, point)};
}

auto CoverageItems::PositionOf(const slang::ast::CoverCrossSymbol& cross) const
    -> hir::CrossId {
  return hir::CrossId{PlaceIn(crosses, cross)};
}

auto CoverageItemsOf(const slang::ast::CovergroupBodySymbol& body)
    -> CoverageItems {
  CoverageItems items;
  for (const auto& point : body.membersOfType<slang::ast::CoverpointSymbol>()) {
    items.coverpoints.push_back(&point);
  }
  for (const auto& cross : body.membersOfType<slang::ast::CoverCrossSymbol>()) {
    items.crosses.push_back(&cross);
  }
  return items;
}

auto ReportNamesOf(const CoverageItems& items, std::size_t unnamed_above)
    -> ReportNames {
  ReportNames names{.coverpoints = {}, .crosses = {}, .unnamed = unnamed_above};
  const auto name_of = [&](const slang::ast::Symbol& item,
                           std::string_view kind) -> std::string {
    if (!item.name.empty()) return std::string{item.name};
    return std::format("{} {}", kind, names.unnamed++);
  };
  for (const slang::ast::CoverpointSymbol* point : items.coverpoints) {
    names.coverpoints.push_back(name_of(*point, "coverpoint"));
  }
  for (const slang::ast::CoverCrossSymbol* cross : items.crosses) {
    names.crosses.push_back(name_of(*cross, "cross"));
  }
  return names;
}

auto BinConstructOf(const slang::ast::CoverageBinSymbol& bin)
    -> hir::BinConstructId {
  const auto& point =
      bin.getParentScope()->asSymbol().as<slang::ast::CoverpointSymbol>();
  std::uint32_t place = 0;
  for (const auto& construct :
       point.membersOfType<slang::ast::CoverageBinSymbol>()) {
    if (&construct == &bin) return hir::BinConstructId{place};
    ++place;
  }
  throw InternalError(
      std::format(
          "BinConstructOf: '{}' is no bin construct of the coverpoint "
          "declaring it -- please report this as a bug",
          bin.name));
}

auto CovergroupDeclaring(const slang::ast::Symbol& item)
    -> const slang::ast::CovergroupType& {
  return item.getParentScope()
      ->asSymbol()
      .getParentScope()
      ->asSymbol()
      .as<slang::ast::CovergroupType>();
}

}  // namespace lyra::lowering::ast_to_hir
