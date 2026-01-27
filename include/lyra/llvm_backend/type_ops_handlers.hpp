#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/type_ops.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace llvm {
class StructType;
class Value;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {

class Context;

// String handler (type_ops_string.cpp)
auto AssignString(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;

// Container handlers (type_ops_container.cpp)
auto AssignDynArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;

// Struct handlers (type_ops_struct.cpp)
auto AssignStruct(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId struct_type_id) -> Result<void>;

// Array handler (type_ops_array.cpp)
auto AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId array_type_id) -> Result<void>;

// Packed handlers (type_ops_packed.cpp)
auto AssignFourState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;

auto AssignTwoState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;

// Union handlers (type_ops_union.cpp)
auto AssignUnion(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId union_type_id) -> Result<void>;

auto ConstructDefaultUnion(
    Context& context, llvm::Value* ptr, TypeId union_type_id) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
