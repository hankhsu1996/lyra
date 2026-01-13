#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/interpreter/intrinsic.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Constant = common::Constant;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerAssignmentExpression(
    const mir::AssignmentExpression& assignment, LirBuilder& builder)
    -> lir::TempRef {
  assert(assignment.value);

  // Handle pop methods specially: they return element but also mutate receiver.
  // For v = q.pop_back(), we need to:
  // 1. Get the element (via pop_back call)
  // 2. Store element to v
  // 3. Delete the element from q
  // 4. Store modified q back
  if (assignment.value->kind == mir::Expression::Kind::kMethodCall) {
    const auto& method_call =
        mir::As<mir::MethodCallExpression>(*assignment.value);
    const auto& receiver = *method_call.receiver;
    bool is_pop =
        (method_call.method == mir::BuiltinMethod::kPopBack ||
         method_call.method == mir::BuiltinMethod::kPopFront);

    if (is_pop && receiver.kind == mir::Expression::Kind::kIdentifier) {
      const auto& ident = mir::As<mir::IdentifierExpression>(receiver);

      // Lower the pop method call (returns element)
      auto element = LowerExpression(*assignment.value, builder);

      // Store element to target through pointer
      const auto& target_type = element->type;
      const auto* target_pointee =
          builder.GetContext()->InternType(target_type);
      auto target_ptr =
          builder.AllocateTemp("ptr", common::Type::Pointer(target_pointee));
      builder.AddInstruction(
          Instruction::ResolveVar(target_ptr, assignment.target.symbol));
      builder.AddInstruction(Instruction::Store(target_ptr, element));

      // Emit delete operation to shrink the queue
      // First, resolve and load the queue variable
      const auto* recv_pointee =
          builder.GetContext()->InternType(receiver.type);
      auto recv_ptr =
          builder.AllocateTemp("ptr", common::Type::Pointer(recv_pointee));
      builder.AddInstruction(Instruction::ResolveVar(recv_ptr, ident.symbol));
      auto recv_temp = builder.AllocateTemp("recv", receiver.type);
      builder.AddInstruction(Instruction::Load(recv_temp, recv_ptr));

      // Create delete index: 0 for pop_front, size-1 for pop_back
      auto del_idx = builder.AllocateTemp("idx", common::Type::Int());
      if (method_call.method == mir::BuiltinMethod::kPopFront) {
        auto zero_lit = builder.InternConstant(common::Constant::Int(0));
        builder.AddInstruction(Instruction::Constant(del_idx, zero_lit));
      } else {
        // pop_back: delete at size - 1
        auto size_temp = builder.AllocateTemp("sz", common::Type::Int());
        void* size_fn =
            interpreter::ResolveIntrinsicMethod(receiver.type.kind, "size");
        builder.AddInstruction(
            Instruction::IntrinsicCall(
                size_fn, recv_temp, {}, size_temp, common::Type::Int()));
        auto one_lit = builder.InternConstant(common::Constant::Int(1));
        auto one_temp = builder.AllocateTemp("one", common::Type::Int());
        builder.AddInstruction(Instruction::Constant(one_temp, one_lit));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kBinarySubtract, del_idx,
                {Operand::Temp(size_temp), Operand::Temp(one_temp)}));
      }

      // Call delete(index) on receiver
      auto del_result = builder.AllocateTemp("del", receiver.type);
      void* delete_fn =
          interpreter::ResolveIntrinsicMethod(receiver.type.kind, "delete");
      builder.AddInstruction(
          Instruction::IntrinsicCall(
              delete_fn, recv_temp, {del_idx}, del_result, receiver.type));

      // Store modified queue back to variable through pointer
      // (reuse the resolved recv_ptr from above)
      builder.AddInstruction(Instruction::Store(recv_ptr, del_result));

      return element;
    }
  }

  auto value = LowerExpression(*assignment.value, builder);

  if (assignment.target.IsHierarchical()) {
    // Hierarchical assignment uses target_symbol directly (flat storage model)
    // Use the value's type for the pointer (types should match after implicit
    // conversion).
    const auto* pointee = builder.GetContext()->InternType(value->type);
    auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
    builder.AddInstruction(
        Instruction::ResolveVar(ptr, assignment.target.target_symbol));
    if (assignment.is_non_blocking) {
      builder.AddInstruction(Instruction::StoreNBA(ptr, value));
    } else {
      builder.AddInstruction(Instruction::Store(ptr, value));
    }
    return value;
  }

  if (assignment.target.IsStructFieldAssignment()) {
    const auto& field_path = assignment.target.field_path;
    const auto& first_field = field_path[0];

    // Check if this is an unpacked struct/union field assignment
    if (!first_field.IsPacked()) {
      // Unpacked struct/union field assignment - use pointer chain
      // For s.inner.x = value: ResolveVar(s) -> ResolveField(inner) ->
      //   ResolveField(x) -> Store
      // WritePointer handles the recursive write-back automatically.
      const auto& base_type = *assignment.target.base_type;

      // Start with pointer to the root variable
      const auto* base_pointee = builder.GetContext()->InternType(base_type);
      auto ptr =
          builder.AllocateTemp("ptr", common::Type::Pointer(base_pointee));
      builder.AddInstruction(
          Instruction::ResolveVar(ptr, assignment.target.symbol));

      // Walk through field path, building pointer chain
      const Type* current_type = &base_type;
      for (const auto& field : field_path) {
        // For union: ALWAYS use index 0 (shared storage)
        size_t field_id = current_type->IsUnpackedUnion() ? 0 : field.index;

        const auto* field_pointee =
            builder.GetContext()->InternType(field.type);
        auto field_ptr = builder.AllocateTemp(
            "ptr_field", common::Type::Pointer(field_pointee));
        builder.AddInstruction(
            Instruction::ResolveField(field_ptr, ptr, field_id));
        ptr = field_ptr;
        current_type = &field.type;
      }

      // Store through the final pointer
      if (assignment.is_non_blocking) {
        builder.AddInstruction(Instruction::StoreNBA(ptr, value));
      } else {
        builder.AddInstruction(Instruction::Store(ptr, value));
      }
      return value;
    }

    // Packed struct field assignment: my_struct.field = value
    // Create literal for field bit offset
    auto offset_temp = builder.AllocateTemp("offset", common::Type::Int());
    auto offset_constant = builder.InternConstant(
        common::Constant::Int(static_cast<int32_t>(*first_field.bit_offset)));
    builder.AddInstruction(Instruction::Constant(offset_temp, offset_constant));

    // Create slice type with field width
    auto slice_type = common::Type::IntegralUnsigned(
        static_cast<uint32_t>(*first_field.bit_width));

    // Emit StorePackedBits instruction
    auto instruction = Instruction::StorePackedBits(
        assignment.target.symbol, offset_temp, value, slice_type);
    builder.AddInstruction(std::move(instruction));
    return value;
  }

  if (assignment.target.IsElementSelect()) {
    if (assignment.target.IsPacked()) {
      // Packed element assignment (possibly multi-dimensional)
      const auto& base_type = *assignment.target.base_type;
      size_t element_width = GetElementWidthAfterIndices(
          base_type, assignment.target.indices.size());
      auto composite_index =
          ComputeCompositeIndex(assignment.target.indices, base_type, builder);
      auto adjusted_index = AdjustForNonZeroLower(
          composite_index, base_type.GetElementLower(), builder);

      // Compute bit_offset = adjusted_index * element_width
      auto bit_offset = builder.AllocateTemp("bit_offset", Type::Int());
      auto width_constant = builder.InternConstant(
          Constant::Int(static_cast<int32_t>(element_width)));
      auto width_temp = builder.AllocateTemp("width", Type::Int());
      builder.AddInstruction(Instruction::Constant(width_temp, width_constant));
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinaryMultiply, bit_offset,
              {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

      auto slice_type =
          Type::IntegralUnsigned(static_cast<uint32_t>(element_width));
      auto instruction = Instruction::StorePackedBits(
          assignment.target.symbol, bit_offset, value, slice_type);
      builder.AddInstruction(std::move(instruction));
    } else {
      // Unpacked array element assignment - use pointer chain
      // For arr[i][j] = value: ResolveVar -> ResolveIndex(i) -> ResolveIndex(j)
      // -> Store The recursive load-modify-store is handled by WritePointer in
      // the interpreter.
      const auto& base_type = *assignment.target.base_type;

      // Lower all indices
      std::vector<TempRef> index_temps;
      for (const auto& idx_expr : assignment.target.indices) {
        index_temps.push_back(LowerExpression(*idx_expr, builder));
      }

      // Build pointer chain starting from the variable
      const auto* arr_pointee = builder.GetContext()->InternType(base_type);
      auto ptr =
          builder.AllocateTemp("ptr", common::Type::Pointer(arr_pointee));
      builder.AddInstruction(
          Instruction::ResolveVar(ptr, assignment.target.symbol));

      // Walk through dimensions, building pointer chain
      const Type* current_type = &base_type;
      for (size_t i = 0; i < index_temps.size(); ++i) {
        const Type& elem_type = current_type->GetElementType();
        const auto* elem_pointee = builder.GetContext()->InternType(elem_type);
        auto elem_ptr = builder.AllocateTemp(
            "ptr_elem", common::Type::Pointer(elem_pointee));
        builder.AddInstruction(
            Instruction::ResolveIndex(elem_ptr, ptr, index_temps[i]));
        ptr = elem_ptr;
        current_type = &elem_type;
      }

      // Store through the final pointer
      if (assignment.is_non_blocking) {
        builder.AddInstruction(Instruction::StoreNBA(ptr, value));
      } else {
        builder.AddInstruction(Instruction::Store(ptr, value));
      }
    }
    return value;
  }

  // Simple variable assignment through pointer
  // Use base_type if available, otherwise use the value's type.
  const auto& var_type = assignment.target.base_type.has_value()
                             ? *assignment.target.base_type
                             : value->type;
  const auto* pointee = builder.GetContext()->InternType(var_type);
  auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
  builder.AddInstruction(
      Instruction::ResolveVar(ptr, assignment.target.symbol));
  if (assignment.is_non_blocking) {
    builder.AddInstruction(Instruction::StoreNBA(ptr, value));
  } else {
    builder.AddInstruction(Instruction::Store(ptr, value));
  }
  return value;
}

}  // namespace lyra::lowering::mir_to_lir
