#include "lyra/lowering/ast_to_hir/net_type.hpp"

#include <slang/ast/types/NetType.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/structural_data_object.hpp"

namespace lyra::lowering::ast_to_hir {

auto TranslateNetType(
    const slang::ast::NetType& net_type, diag::SourceSpan span)
    -> diag::Result<hir::NetType> {
  switch (net_type.netKind) {
    case slang::ast::NetType::Wire:
      return hir::NetType::kWire;
    case slang::ast::NetType::Tri:
      return hir::NetType::kTri;
    case slang::ast::NetType::WAnd:
      return hir::NetType::kWand;
    case slang::ast::NetType::TriAnd:
      return hir::NetType::kTriand;
    case slang::ast::NetType::WOr:
      return hir::NetType::kWor;
    case slang::ast::NetType::TriOr:
      return hir::NetType::kTrior;
    case slang::ast::NetType::Tri0:
      return hir::NetType::kTri0;
    case slang::ast::NetType::Tri1:
      return hir::NetType::kTri1;
    case slang::ast::NetType::Supply0:
      return hir::NetType::kSupply0;
    case slang::ast::NetType::Supply1:
      return hir::NetType::kSupply1;
    case slang::ast::NetType::UWire:
      return hir::NetType::kUwire;
    case slang::ast::NetType::TriReg:
      return hir::NetType::kTrireg;
    case slang::ast::NetType::Interconnect:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTypeKind,
          "an interconnect net (LRM 6.6.8) is not yet supported");
    case slang::ast::NetType::UserDefined:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTypeKind,
          "a user-defined nettype (LRM 6.6.7) is not yet supported");
    case slang::ast::NetType::Unknown:
      break;
  }
  // Every net type the language has is above, so an unknown one is a
  // declaration the front end rejected and lowering should never have reached.
  throw InternalError("TranslateNetType: the net has no net type");
}

}  // namespace lyra::lowering::ast_to_hir
