#include "lyra/mir/keeper.hpp"

#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto KeeperEntriesOf(const TypePool& types, TypeId keeper)
    -> std::optional<KeeperEntries> {
  const auto* library = types.Get(keeper).As<RuntimeLibraryType>();
  if (library == nullptr) {
    return std::nullopt;
  }
  switch (library->kind) {
    case RuntimeLibraryKind::kDeferredEffects:
      return KeeperEntries{
          .allocate = support::BuiltinFn::kDeferredEffectsAllocate,
          .commit = support::BuiltinFn::kDeferredEffectsCommit};
    case RuntimeLibraryKind::kViolationReportKeeper:
      return KeeperEntries{
          .allocate = support::BuiltinFn::kViolationReportsAllocate,
          .commit = support::BuiltinFn::kViolationReportsCommit};
    case RuntimeLibraryKind::kBitVec:
    case RuntimeLibraryKind::kEnumeration:
    case RuntimeLibraryKind::kPrintItem:
    case RuntimeLibraryKind::kFormatSpec:
    case RuntimeLibraryKind::kFormatArg:
    case RuntimeLibraryKind::kChannelCancellation:
    case RuntimeLibraryKind::kTimeFormat:
    case RuntimeLibraryKind::kTrigger:
    case RuntimeLibraryKind::kObservation:
    case RuntimeLibraryKind::kReadReport:
    case RuntimeLibraryKind::kWait:
    case RuntimeLibraryKind::kLeasedReference:
    case RuntimeLibraryKind::kHierarchySegment:
    case RuntimeLibraryKind::kDpiBitBuffer:
    case RuntimeLibraryKind::kDpiLogicBuffer:
    case RuntimeLibraryKind::kDpiBitChunk:
    case RuntimeLibraryKind::kDpiLogicChunk:
    case RuntimeLibraryKind::kDpiOpenArray:
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
    case RuntimeLibraryKind::kWatchedPlace:
    case RuntimeLibraryKind::kCancellationTarget:
    case RuntimeLibraryKind::kControlEffect:
    case RuntimeLibraryKind::kTimer:
    case RuntimeLibraryKind::kObjectDefinition:
    case RuntimeLibraryKind::kScopeInfo:
    case RuntimeLibraryKind::kScopeCallable:
    case RuntimeLibraryKind::kClassObjectInfo:
    case RuntimeLibraryKind::kInterfaceRow:
    case RuntimeLibraryKind::kConstructionTable:
    case RuntimeLibraryKind::kInstantiation:
    case RuntimeLibraryKind::kGeneratePlaces:
    case RuntimeLibraryKind::kDumpEntry:
    case RuntimeLibraryKind::kDumpBlock:
      return std::nullopt;
  }
  throw InternalError("mir::KeeperEntriesOf: unknown runtime library kind");
}

}  // namespace lyra::mir
