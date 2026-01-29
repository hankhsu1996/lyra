#include <expected>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId array_type_id) -> Result<void> {
  const auto& types = context.GetTypeArena();

  if (NeedsFieldByField(array_type_id, types)) {
    if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          "array literal with managed elements not yet supported",
          UnsupportedCategory::kFeature));
    }
    auto source_place_id = std::get<mir::PlaceId>(source.payload);
    return CommitArrayFieldByField(
        context, target, source_place_id, array_type_id, policy);
  }

  auto val_or_err = LowerOperandRaw(context, source);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;
  CommitPackedValueRaw(context, target, val);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
