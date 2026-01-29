#include <expected>
#include <variant>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignStruct(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId struct_type_id) -> Result<void> {
  const auto& types = context.GetTypeArena();

  // Container handles (dynarray/queue) require deep copy not yet implemented
  if (TypeContainsManaged(struct_type_id, types) &&
      !NeedsFieldByField(struct_type_id, types)) {
    // Contains containers but not strings - this is the deferred case
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "unpacked struct assignment with container fields "
        "(dynamic array/queue) not yet supported",
        UnsupportedCategory::kFeature));
  }

  // Structs with string fields require field-by-field assignment
  if (NeedsFieldByField(struct_type_id, types)) {
    // Source must be a place for struct assignment
    if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
      throw common::InternalError(
          "AssignStruct",
          "struct assignment source must be a place, not a constant");
    }
    auto src_place_id = std::get<mir::PlaceId>(source.payload);
    return CommitStructFieldByField(
        context, target, src_place_id, struct_type_id, policy);
  }

  // No managed fields: fast aggregate load/store
  auto val_result = LowerOperandRaw(context, source);
  if (!val_result) return std::unexpected(val_result.error());
  llvm::Value* val = *val_result;
  CommitPackedValueRaw(context, target, val);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
