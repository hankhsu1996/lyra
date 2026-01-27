#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId array_type_id) -> Result<void> {
  const auto& types = context.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  const Type& elem_type = types[arr_info.element_type];

  if (elem_type.Kind() == TypeKind::kDynamicArray ||
      elem_type.Kind() == TypeKind::kQueue ||
      elem_type.Kind() == TypeKind::kString) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "unpacked array assignment with owned-handle elements "
        "(dynamic array or string) not yet supported",
        UnsupportedCategory::kFeature));
  }

  auto wt_or_err = context.GetWriteTarget(target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());
  const WriteTarget& wt = *wt_or_err;

  auto val_or_err = LowerOperandRaw(context, source);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;
  detail::StorePackedToWriteTarget(context, val, wt);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
