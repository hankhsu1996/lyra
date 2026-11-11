#include "lyra/runtime/dump_scope_kind.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::runtime {

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

auto DumpScopeKindOf(support::ElaboratedScopeKind kind) -> DumpScopeKind {
  switch (kind) {
    case support::ElaboratedScopeKind::kModule:
    case support::ElaboratedScopeKind::kInterface:
    case support::ElaboratedScopeKind::kProgram:
    case support::ElaboratedScopeKind::kModuleArray:
    case support::ElaboratedScopeKind::kInterfaceArray:
    case support::ElaboratedScopeKind::kProgramArray:
      return DumpScopeKind::kModule;
    case support::ElaboratedScopeKind::kGenerateBlock:
    case support::ElaboratedScopeKind::kGenerateLoop:
      return DumpScopeKind::kBegin;
    case support::ElaboratedScopeKind::kDesignRoot:
      throw InternalError(
          "DumpScopeKindOf: a dump declares no scope for the design's root -- "
          "please report this as a bug");
  }
  throw InternalError("DumpScopeKindOf: unknown kind");
}

auto DumpScopeKindOf(support::LexicalScopeKind kind) -> DumpScopeKind {
  switch (kind) {
    case support::LexicalScopeKind::kBegin:
    case support::LexicalScopeKind::kFor:
    case support::LexicalScopeKind::kForeach:
      return DumpScopeKind::kBegin;
    case support::LexicalScopeKind::kForkJoin:
    case support::LexicalScopeKind::kForkJoinAny:
    case support::LexicalScopeKind::kForkJoinNone:
      return DumpScopeKind::kFork;
    case support::LexicalScopeKind::kTask:
      return DumpScopeKind::kTask;
    case support::LexicalScopeKind::kFunction:
      return DumpScopeKind::kFunction;
    case support::LexicalScopeKind::kClass:
    case support::LexicalScopeKind::kClockingBlock:
      throw InternalError(
          "DumpScopeKindOf: a dump declares no scope for a class or a "
          "clocking block -- please report this as a bug");
  }
  throw InternalError("DumpScopeKindOf: unknown kind");
}

}  // namespace lyra::runtime
