#pragma once

#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/type_ops.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace llvm {
class StructType;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {

class Context;

// String handler (type_ops_string.cpp)
void AssignString(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id);

// Container handlers (type_ops_container.cpp)
void AssignDynArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id);

// Struct handlers (type_ops_struct.cpp)
void AssignStruct(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId struct_type_id);

// Array handler (type_ops_array.cpp)
void AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId array_type_id);

// Packed handlers (type_ops_packed.cpp)
void AssignFourState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    llvm::StructType* struct_type);

void AssignTwoState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId type_id);

}  // namespace lyra::lowering::mir_to_llvm
