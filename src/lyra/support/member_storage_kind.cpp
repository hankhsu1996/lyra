#include "lyra/support/member_storage_kind.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto MemberStorageKindName(MemberStorageKind kind) -> std::string_view {
  switch (kind) {
    case MemberStorageKind::kObservableCell:
      return "cell";
    case MemberStorageKind::kResolvedNet:
      return "net";
    case MemberStorageKind::kSampledHistory:
      return "sampled_history";
    case MemberStorageKind::kValueCell:
      return "value_cell";
    case MemberStorageKind::kInlineValue:
      return "inline_value";
    case MemberStorageKind::kBorrowedHandle:
      return "borrowed_handle";
    case MemberStorageKind::kReference:
      return "reference";
    case MemberStorageKind::kAnyReference:
      return "any_reference";
    case MemberStorageKind::kWeakReference:
      return "weak_reference";
    case MemberStorageKind::kSharedPointer:
      return "shared_pointer";
    case MemberStorageKind::kCancellationTarget:
      return "cancellation_target";
    case MemberStorageKind::kChannelCancellation:
      return "channel_cancellation";
    case MemberStorageKind::kEvaluationAttempts:
      return "evaluation_attempts";
    case MemberStorageKind::kTimer:
      return "timer";
    case MemberStorageKind::kLoopBlocks:
      return "loop_blocks";
  }
  throw InternalError("member storage kind: unknown kind");
}

}  // namespace lyra::support
