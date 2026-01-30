#include "lyra/llvm_backend/instruction/value_plusargs.hpp"

#include <expected>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

enum class PlusargsOutputKind { kInt32, kString };

auto GetPlusargsOutputKind(const TypeArena& types, TypeId output_type)
    -> PlusargsOutputKind {
  const Type& type = types[output_type];
  if (type.Kind() == TypeKind::kString) {
    return PlusargsOutputKind::kString;
  }
  return PlusargsOutputKind::kInt32;
}

}  // namespace

auto LowerValuePlusargs(Context& context, const mir::ValuePlusargs& vp)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Evaluate query string
  auto query_or_err = LowerOperand(context, vp.query);
  if (!query_or_err) return std::unexpected(query_or_err.error());

  // Get output pointer (allowed here - we're in statement layer)
  auto output_ptr = context.GetPlacePointer(vp.output);
  if (!output_ptr) return std::unexpected(output_ptr.error());

  // Call runtime helper
  llvm::Value* success = nullptr;
  auto kind = GetPlusargsOutputKind(types, vp.output_type);
  if (kind == PlusargsOutputKind::kString) {
    success = builder.CreateCall(
        context.GetLyraPlusargsValueString(),
        {context.GetEnginePointer(), *query_or_err, *output_ptr});
  } else {
    success = builder.CreateCall(
        context.GetLyraPlusargsValueInt(),
        {context.GetEnginePointer(), *query_or_err, *output_ptr});
  }

  // Store success result to dest place
  // Success is a trivial i32, use CommitValue with kMove (no ownership
  // concerns)
  TypeId dest_type = mir::TypeOfPlace(types, arena[vp.dest]);
  return CommitValue(
      context, vp.dest, success, dest_type, OwnershipPolicy::kMove);
}

}  // namespace lyra::lowering::mir_to_llvm
