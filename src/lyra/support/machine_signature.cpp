#include "lyra/support/machine_signature.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto MachineKindName(MachineKind kind) -> std::string_view {
  switch (kind) {
    case MachineKind::kVoid:
      return "void";
    case MachineKind::kAddress:
      return "pointer";
    case MachineKind::kTruth:
      return "bool";
    case MachineKind::kInt8:
      return "int8";
    case MachineKind::kInt32:
      return "int32";
    case MachineKind::kInt64:
      return "int64";
    case MachineKind::kFloat:
      return "float";
    case MachineKind::kDouble:
      return "double";
    case MachineKind::kAggregate:
      return "aggregate";
  }
  throw InternalError("support::MachineKindName: unknown MachineKind");
}

}  // namespace lyra::support
