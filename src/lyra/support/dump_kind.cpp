#include "lyra/support/dump_kind.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto DumpVarKindKeyword(DumpVarKind kind) -> std::string_view {
  switch (kind) {
    case DumpVarKind::kEvent:
      return "event";
    case DumpVarKind::kInteger:
      return "integer";
    case DumpVarKind::kReal:
      return "real";
    case DumpVarKind::kRealtime:
      return "realtime";
    case DumpVarKind::kReg:
      return "reg";
    case DumpVarKind::kSupply0:
      return "supply0";
    case DumpVarKind::kSupply1:
      return "supply1";
    case DumpVarKind::kTime:
      return "time";
    case DumpVarKind::kTri:
      return "tri";
    case DumpVarKind::kTriand:
      return "triand";
    case DumpVarKind::kTrior:
      return "trior";
    case DumpVarKind::kTrireg:
      return "trireg";
    case DumpVarKind::kTri0:
      return "tri0";
    case DumpVarKind::kTri1:
      return "tri1";
    case DumpVarKind::kWand:
      return "wand";
    case DumpVarKind::kWire:
      return "wire";
    case DumpVarKind::kWor:
      return "wor";
  }
  throw InternalError("DumpVarKindKeyword: unknown kind");
}

auto DumpValueFormOf(DumpVarKind kind) -> DumpValueForm {
  switch (kind) {
    case DumpVarKind::kEvent:
      return DumpValueForm::kTriggered;
    case DumpVarKind::kReal:
    case DumpVarKind::kRealtime:
      return DumpValueForm::kReal;
    case DumpVarKind::kInteger:
    case DumpVarKind::kReg:
    case DumpVarKind::kSupply0:
    case DumpVarKind::kSupply1:
    case DumpVarKind::kTime:
    case DumpVarKind::kTri:
    case DumpVarKind::kTriand:
    case DumpVarKind::kTrior:
    case DumpVarKind::kTrireg:
    case DumpVarKind::kTri0:
    case DumpVarKind::kTri1:
    case DumpVarKind::kWand:
    case DumpVarKind::kWire:
    case DumpVarKind::kWor:
      return DumpValueForm::kBits;
  }
  throw InternalError("DumpValueFormOf: unknown kind");
}

auto DumpScopeKindKeyword(DumpScopeKind kind) -> std::string_view {
  switch (kind) {
    case DumpScopeKind::kModule:
      return "module";
    case DumpScopeKind::kTask:
      return "task";
    case DumpScopeKind::kFunction:
      return "function";
    case DumpScopeKind::kBegin:
      return "begin";
    case DumpScopeKind::kFork:
      return "fork";
  }
  throw InternalError("DumpScopeKindKeyword: unknown kind");
}

}  // namespace lyra::support
