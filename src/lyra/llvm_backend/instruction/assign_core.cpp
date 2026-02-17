#include "lyra/llvm_backend/instruction/assign_core.hpp"

#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

auto LowerRhsRaw(
    Context& context, const mir::RightHandSide& rhs, mir::PlaceId target)
    -> Result<llvm::Value*> {
  return std::visit(
      common::Overloaded{
          [&](const mir::Operand& operand) -> Result<llvm::Value*> {
            return LowerOperandRaw(context, operand);
          },
          [&](const mir::Rvalue& rvalue) -> Result<llvm::Value*> {
            const auto& arena = context.GetMirArena();
            const auto& types = context.GetTypeArena();
            TypeId result_type = mir::TypeOfPlace(types, arena[target]);
            auto rv_result = LowerRvalue(context, rvalue, result_type);
            if (!rv_result) return std::unexpected(rv_result.error());
            // For 4-state, pack into struct
            if (IsPacked(types[result_type]) &&
                context.IsPackedFourState(types[result_type])) {
              auto storage_type_or_err = context.GetPlaceLlvmType(target);
              if (!storage_type_or_err)
                return std::unexpected(storage_type_or_err.error());
              auto* struct_type =
                  llvm::cast<llvm::StructType>(*storage_type_or_err);
              auto* elem_type = struct_type->getElementType(0);
              auto& builder = context.GetBuilder();
              llvm::Value* value =
                  builder.CreateZExtOrTrunc(rv_result->value, elem_type);
              llvm::Value* unknown = rv_result->unknown;
              if (unknown == nullptr) {
                unknown = llvm::ConstantInt::get(elem_type, 0);
              } else {
                unknown = builder.CreateZExtOrTrunc(unknown, elem_type);
              }
              return PackFourState(builder, struct_type, value, unknown);
            }
            return rv_result->value;
          },
      },
      rhs);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
