#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
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
        (method_call.method_name == "pop_back" ||
         method_call.method_name == "pop_front");

    if (is_pop && receiver.kind == mir::Expression::Kind::kIdentifier) {
      const auto& ident = mir::As<mir::IdentifierExpression>(receiver);

      // Lower the pop method call (returns element)
      auto element = LowerExpression(*assignment.value, builder);

      // Store element to target
      auto store_elem = Instruction::StoreVariable(
          assignment.target.symbol, element, assignment.is_non_blocking);
      builder.AddInstruction(std::move(store_elem));

      // Emit delete operation to shrink the queue
      auto recv_temp = builder.AllocateTemp("recv", receiver.type);
      builder.AddInstruction(
          Instruction::Basic(IK::kLoadVariable, recv_temp, ident.symbol));

      // Create delete index: 0 for pop_front, size-1 for pop_back
      auto del_idx = builder.AllocateTemp("idx", common::Type::Int());
      if (method_call.method_name == "pop_front") {
        auto zero_lit = builder.InternConstant(common::Constant::Int(0));
        builder.AddInstruction(
            Instruction::Basic(IK::kConstant, del_idx, zero_lit));
      } else {
        // pop_back: delete at size - 1
        auto size_temp = builder.AllocateTemp("sz", common::Type::Int());
        builder.AddInstruction(
            Instruction::MethodCall(
                "size", recv_temp, {}, size_temp, common::Type::Int(), 1, {}));
        auto one_lit = builder.InternConstant(common::Constant::Int(1));
        auto one_temp = builder.AllocateTemp("one", common::Type::Int());
        builder.AddInstruction(
            Instruction::Basic(IK::kConstant, one_temp, one_lit));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kBinarySubtract, del_idx,
                {Operand::Temp(size_temp), Operand::Temp(one_temp)}));
      }

      // Call delete(index) on receiver
      std::vector<Operand> del_args = {Operand::Temp(del_idx)};
      auto del_result = builder.AllocateTemp("del", receiver.type);
      builder.AddInstruction(
          Instruction::MethodCall(
              "delete", recv_temp, std::move(del_args), del_result,
              receiver.type, 1, {}));

      // Store modified queue back to variable
      auto store_queue =
          Instruction::StoreVariable(ident.symbol, del_result, false);
      builder.AddInstruction(std::move(store_queue));

      return element;
    }
  }

  auto value = LowerExpression(*assignment.value, builder);

  if (assignment.target.IsHierarchical()) {
    // Hierarchical assignment uses target_symbol directly (flat storage model)
    auto instruction = Instruction::StoreVariable(
        assignment.target.target_symbol, value, assignment.is_non_blocking);
    builder.AddInstruction(std::move(instruction));
    return value;
  }

  if (assignment.target.IsStructFieldAssignment()) {
    const auto& field_path = assignment.target.field_path;
    const auto& first_field = field_path[0];

    // Check if this is an unpacked struct/union field assignment
    if (!first_field.IsPacked()) {
      // Handle nested unpacked struct/union field assignment
      // For s.inner.x = value:
      //   1. Load intermediate aggregates
      //   2. Store value into innermost field
      //   3. Store modified aggregates back up the chain

      if (field_path.size() == 1) {
        // Single-level: direct store (optimized path)
        size_t storage_index = assignment.target.base_type->IsUnpackedUnion()
                                   ? 0
                                   : first_field.index;
        auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
        auto index_constant = builder.InternConstant(
            common::Constant::Int(static_cast<int32_t>(storage_index)));
        builder.AddInstruction(
            Instruction::Basic(IK::kConstant, index_temp, index_constant));

        auto instruction = Instruction::StoreElement(
            Operand::Variable(assignment.target.symbol), index_temp, value,
            assignment.is_non_blocking);
        builder.AddInstruction(std::move(instruction));
      } else {
        // Multi-level nested access: chain loads and stores
        // Load intermediate aggregates from root to second-to-last
        std::vector<TempRef> temps;
        temps.reserve(field_path.size() - 1);

        // First load from root variable
        auto first_idx_temp = builder.AllocateTemp("idx", common::Type::Int());
        auto first_idx_constant = builder.InternConstant(
            common::Constant::Int(
                static_cast<int32_t>(
                    assignment.target.base_type->IsUnpackedUnion()
                        ? 0
                        : field_path[0].index)));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kConstant, first_idx_temp, first_idx_constant));

        auto first_temp = builder.AllocateTemp("agg", field_path[0].type);
        builder.AddInstruction(
            Instruction::LoadElement(
                first_temp, Operand::Variable(assignment.target.symbol),
                first_idx_temp, field_path[0].type));
        temps.push_back(first_temp);

        // Load remaining intermediate aggregates
        for (size_t i = 1; i < field_path.size() - 1; ++i) {
          auto idx_temp = builder.AllocateTemp("idx", common::Type::Int());
          // Use parent's type to determine if it's a union
          bool parent_is_union = field_path[i - 1].type.IsUnpackedUnion();
          auto idx_constant = builder.InternConstant(
              common::Constant::Int(
                  static_cast<int32_t>(
                      parent_is_union ? 0 : field_path[i].index)));
          builder.AddInstruction(
              Instruction::Basic(IK::kConstant, idx_temp, idx_constant));

          auto temp = builder.AllocateTemp("agg", field_path[i].type);
          builder.AddInstruction(
              Instruction::LoadElement(
                  temp, Operand::Temp(temps.back()), idx_temp,
                  field_path[i].type));
          temps.push_back(temp);
        }

        // Store value into innermost field
        const auto& last_field = field_path.back();
        auto last_idx_temp = builder.AllocateTemp("idx", common::Type::Int());
        // Use parent's type (second-to-last in path) to determine if union
        bool parent_is_union =
            field_path[field_path.size() - 2].type.IsUnpackedUnion();
        auto last_idx_constant = builder.InternConstant(
            common::Constant::Int(
                static_cast<int32_t>(parent_is_union ? 0 : last_field.index)));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kConstant, last_idx_temp, last_idx_constant));

        builder.AddInstruction(
            Instruction::StoreElement(
                Operand::Temp(temps.back()), last_idx_temp, value,
                assignment.is_non_blocking));

        // Store modified aggregates back up the chain
        for (size_t i = temps.size() - 1; i > 0; --i) {
          auto idx_temp = builder.AllocateTemp("idx", common::Type::Int());
          // Use grandparent's type to determine if parent is a union
          bool gparent_is_union =
              (i >= 2) ? field_path[i - 2].type.IsUnpackedUnion()
                       : assignment.target.base_type->IsUnpackedUnion();
          auto idx_constant = builder.InternConstant(
              common::Constant::Int(
                  static_cast<int32_t>(
                      gparent_is_union ? 0 : field_path[i].index)));
          builder.AddInstruction(
              Instruction::Basic(IK::kConstant, idx_temp, idx_constant));

          builder.AddInstruction(
              Instruction::StoreElement(
                  Operand::Temp(temps[i - 1]), idx_temp, temps[i],
                  assignment.is_non_blocking));
        }

        // Store first intermediate back to root variable
        auto root_idx_temp = builder.AllocateTemp("idx", common::Type::Int());
        auto root_idx_constant = builder.InternConstant(
            common::Constant::Int(
                static_cast<int32_t>(
                    assignment.target.base_type->IsUnpackedUnion()
                        ? 0
                        : field_path[0].index)));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kConstant, root_idx_temp, root_idx_constant));

        builder.AddInstruction(
            Instruction::StoreElement(
                Operand::Variable(assignment.target.symbol), root_idx_temp,
                temps[0], assignment.is_non_blocking));
      }
      return value;
    }

    // Packed struct field assignment: my_struct.field = value
    // Create literal for field bit offset
    auto offset_temp = builder.AllocateTemp("offset", common::Type::Int());
    auto offset_constant = builder.InternConstant(
        common::Constant::Int(static_cast<int32_t>(*first_field.bit_offset)));
    builder.AddInstruction(
        Instruction::Basic(IK::kConstant, offset_temp, offset_constant));

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
      builder.AddInstruction(
          Instruction::Basic(IK::kConstant, width_temp, width_constant));
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
      // Unpacked array element assignment
      size_t num_indices = assignment.target.indices.size();

      // Lower all indices
      std::vector<TempRef> index_temps;
      for (const auto& idx_expr : assignment.target.indices) {
        index_temps.push_back(LowerExpression(*idx_expr, builder));
      }

      if (num_indices == 1) {
        // 1D: simple store
        auto instruction = Instruction::StoreElement(
            Operand::Variable(assignment.target.symbol), index_temps[0], value,
            assignment.is_non_blocking);
        builder.AddInstruction(std::move(instruction));
      } else {
        // Multi-dimensional: copy-modify-store pattern
        // For arr[i][j] = value: load arr[i], modify [j], store back
        const auto& base_type = *assignment.target.base_type;

        // Load intermediate arrays (all but the last index)
        std::vector<TempRef> intermediate_temps;
        const Type* current_type = &base_type;

        for (size_t i = 0; i < num_indices - 1; ++i) {
          const Type& element_type = current_type->GetElementType();
          auto temp = builder.AllocateTemp("arr", element_type);

          if (i == 0) {
            // First level: load from variable
            auto instr = Instruction::LoadElement(
                temp, Operand::Variable(assignment.target.symbol),
                index_temps[i], element_type);
            builder.AddInstruction(std::move(instr));
          } else {
            // Subsequent levels: load from temp
            auto instr = Instruction::LoadElement(
                temp, Operand::Temp(intermediate_temps.back()), index_temps[i],
                element_type);
            builder.AddInstruction(std::move(instr));
          }

          intermediate_temps.push_back(temp);
          current_type = &element_type;
        }

        // Store value to the innermost array using the last index
        auto store_instr = Instruction::StoreElement(
            Operand::Temp(intermediate_temps.back()), index_temps.back(),
            value);
        builder.AddInstruction(std::move(store_instr));

        // Store back intermediate arrays in reverse order (3D+).
        // For arr[i][j][k] = v (3D), this stores temp0[j] = temp1.
        // Store-backs may be redundant due to shared_ptr semantics in
        // RuntimeValue, but kept for correctness regardless of storage
        // implementation.
        auto num_intermediates = static_cast<int>(intermediate_temps.size());
        for (int i = num_intermediates - 1; i >= 1; --i) {
          auto instr = Instruction::StoreElement(
              Operand::Temp(intermediate_temps[i - 1]), index_temps[i],
              intermediate_temps[i]);
          builder.AddInstruction(std::move(instr));
        }

        // Store back to the base variable
        auto final_store = Instruction::StoreElement(
            Operand::Variable(assignment.target.symbol), index_temps[0],
            intermediate_temps[0], assignment.is_non_blocking);
        builder.AddInstruction(std::move(final_store));
      }
    }
    return value;
  }

  // Simple variable assignment
  auto instruction = Instruction::StoreVariable(
      assignment.target.symbol, value, assignment.is_non_blocking);
  builder.AddInstruction(std::move(instruction));
  return value;
}

}  // namespace lyra::lowering::mir_to_lir
