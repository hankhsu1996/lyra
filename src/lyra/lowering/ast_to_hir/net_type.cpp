#include "lyra/lowering/ast_to_hir/net_type.hpp"

#include <slang/ast/types/NetType.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/support/data_object.hpp"

namespace lyra::lowering::ast_to_hir {

auto TranslateNetType(
    const slang::ast::NetType& net_type, diag::SourceSpan span)
    -> diag::Result<support::NetType> {
  switch (net_type.netKind) {
    case slang::ast::NetType::Wire:
      return support::NetType::kWire;
    case slang::ast::NetType::Tri:
      return support::NetType::kTri;
    case slang::ast::NetType::WAnd:
      return support::NetType::kWand;
    case slang::ast::NetType::TriAnd:
      return support::NetType::kTriand;
    case slang::ast::NetType::WOr:
      return support::NetType::kWor;
    case slang::ast::NetType::TriOr:
      return support::NetType::kTrior;
    case slang::ast::NetType::Tri0:
      return support::NetType::kTri0;
    case slang::ast::NetType::Tri1:
      return support::NetType::kTri1;
    case slang::ast::NetType::Supply0:
      return support::NetType::kSupply0;
    case slang::ast::NetType::Supply1:
      return support::NetType::kSupply1;
    case slang::ast::NetType::UWire:
      return support::NetType::kUwire;
    case slang::ast::NetType::TriReg:
      return support::NetType::kTrireg;
    case slang::ast::NetType::Interconnect:
      // LRM 6.6.8.
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTypeKind,
          "an 'interconnect' net is not yet supported");
    case slang::ast::NetType::UserDefined:
      // LRM 6.6.7.
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTypeKind,
          "a user-defined 'nettype' is not yet supported");
    case slang::ast::NetType::Unknown:
      break;
  }
  // Every net type the language has is above, so an unknown one is a
  // declaration the front end rejected and lowering should never have reached.
  throw InternalError("TranslateNetType: the net has no net type");
}

}  // namespace lyra::lowering::ast_to_hir
