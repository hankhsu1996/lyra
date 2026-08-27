#include "lyra/lowering/ast_to_hir/net_type.hpp"

#include <optional>

#include <slang/ast/types/NetType.h>

#include "lyra/hir/structural_data_object.hpp"

namespace lyra::lowering::ast_to_hir {

auto TranslateNetType(const slang::ast::NetType& net_type)
    -> std::optional<hir::NetType> {
  switch (net_type.netKind) {
    case slang::ast::NetType::Wire:
      return hir::NetType::kWire;
    case slang::ast::NetType::Tri:
      return hir::NetType::kTri;
    default:
      return std::nullopt;
  }
}

}  // namespace lyra::lowering::ast_to_hir
