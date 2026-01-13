#include "lyra/interpreter/instruction/memory.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto HandleMemoryOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  switch (instr.kind) {
    case lir::InstructionKind::kConstant: {
      assert(instr.constant.has_value());
      assert(instr.result.has_value());

      RuntimeValue value = RuntimeValue::FromConstant(*instr.constant);
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveVar: {
      assert(instr.symbol.has_value());
      assert(instr.result.has_value());

      // Create a pointer value pointing to the variable
      auto ptr = PointerValue::Var(*instr.symbol);
      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(instr.result.value(), RuntimeValue::Pointer(ptr_type, ptr));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoad: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(ptr_value.IsPointer());

      RuntimeValue loaded = ctx.ReadPointer(ptr_value.AsPointer());
      ctx.WriteTemp(instr.result.value(), std::move(loaded));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStore: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.WritePointer(ptr_value.AsPointer(), value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreNBA: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.WritePointer(ptr_value.AsPointer(), value, true);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveIndex: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 2);

      const auto& base_ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(base_ptr_value.IsPointer());

      const auto& index_value = ctx.GetTemp(instr.temp_operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      // Create IndexPointer with base and index
      // Note: lower_bound adjustment happens in ReadPointer/WritePointer
      auto base_ptr = base_ptr_value.AsPointerStorage();
      auto elem_ptr = PointerValue::Index(base_ptr, index);

      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(), RuntimeValue::Pointer(ptr_type, elem_ptr));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveField: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& base_ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(base_ptr_value.IsPointer());

      // field_id is stored in lower_bound field
      auto field_id = static_cast<size_t>(instr.lower_bound);

      // Create FieldPointer with base and field_id
      auto base_ptr = base_ptr_value.AsPointerStorage();
      auto field_ptr = PointerValue::Field(base_ptr, field_id);

      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(), RuntimeValue::Pointer(ptr_type, field_ptr));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveSlice: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 3);

      const auto& base_ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(base_ptr_value.IsPointer());

      const auto& offset_value = ctx.GetTemp(instr.temp_operands[1]);
      const auto& width_value = ctx.GetTemp(instr.temp_operands[2]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      assert(!width_value.IsWide() && "bit width cannot be wide");
      auto offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());
      auto width = static_cast<size_t>(width_value.AsNarrow().AsInt64());

      // Create SlicePointer with base, offset, and width
      auto base_ptr = base_ptr_value.AsPointerStorage();
      auto slice_ptr = PointerValue::Slice(base_ptr, offset, width);

      auto slice_ref_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Pointer(slice_ref_type, slice_ptr));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadSlice: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& slice_ref_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(slice_ref_value.IsSliceRef());

      // Get signedness from SliceRef pointee type
      const auto& pointee_type = slice_ref_value.type.GetSliceRefPointeeType();
      bool is_signed = pointee_type.IsSigned();

      RuntimeValue loaded = ctx.ReadPointer(slice_ref_value.AsPointer());

      // Apply correct signedness (ReadPointer returns unsigned)
      if (is_signed && !loaded.IsWide()) {
        size_t width = pointee_type.GetBitWidth();
        loaded =
            RuntimeValue::IntegralSigned(loaded.AsNarrow().AsInt64(), width);
      } else if (is_signed && loaded.IsWide()) {
        size_t width = pointee_type.GetBitWidth();
        loaded = RuntimeValue::IntegralWide(loaded.AsWideBit(), width, true);
      }

      ctx.WriteTemp(instr.result.value(), std::move(loaded));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreSlice: {
      assert(instr.temp_operands.size() == 2);

      const auto& slice_ref_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(slice_ref_value.IsSliceRef());

      ctx.WritePointer(slice_ref_value.AsPointer(), value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElement: {
      assert(instr.operands.size() == 3);

      auto index_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = ctx.GetTemp(instr.operands[2].AsTempRef());

      ctx.StoreElement(instr.operands[0], index, element_value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElementNonBlocking: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      auto index_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = ctx.GetTemp(instr.operands[2].AsTempRef());

      ctx.StoreElement(instr.operands[0], index, element_value, true);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kAllocate: {
      assert(instr.result.has_value());
      const auto& result_type = (*instr.result)->type;

      if (instr.operands.empty()) {
        // Fixed-size allocation: use DefaultValueForType
        auto value = RuntimeValue::DefaultValueForType(result_type);
        ctx.WriteTemp(instr.result.value(), std::move(value));
      } else {
        // Dynamic allocation with size (and optional init)
        auto size_val = ctx.GetOperandValue(instr.operands[0]);
        auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

        const auto& dyn_data =
            std::get<common::DynamicArrayData>(result_type.data);
        const auto& elem_type = *dyn_data.element_type;

        std::vector<RuntimeValue> elements;
        elements.reserve(size);

        if (instr.operands.size() > 1) {
          const auto& init = ctx.GetOperandValue(instr.operands[1]);
          const auto& init_arr = init.AsArray();
          for (size_t i = 0; i < size; ++i) {
            if (i < init_arr.size()) {
              elements.push_back(init_arr[i].DeepCopy());
            } else {
              elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
            }
          }
        } else {
          for (size_t i = 0; i < size; ++i) {
            elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
          }
        }

        ctx.WriteTemp(
            instr.result.value(),
            RuntimeValue::Array(result_type, std::move(elements)));
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kExtractBits: {
      assert(instr.temp_operands.size() == 2);
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = ctx.GetTemp(instr.temp_operands[0]);
      auto offset_value = ctx.GetTemp(instr.temp_operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
      assert(result_type.IsBitvector());
      size_t width = result_type.GetBitWidth();
      bool is_signed = result_type.IsSigned();

      RuntimeValue result;
      if (width <= 64) {
        uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
        uint64_t extracted = 0;
        if (value.IsWide()) {
          auto shifted = value.AsWideBit().ShiftRightLogical(bit_offset);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (value.AsNarrow().AsUInt64() >> bit_offset) & mask;
        }
        result = is_signed ? RuntimeValue::IntegralSigned(
                                 static_cast<int64_t>(extracted), width)
                           : RuntimeValue::IntegralUnsigned(extracted, width);
      } else {
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(bit_offset, width);
        result =
            RuntimeValue::IntegralWide(std::move(extracted), width, is_signed);
      }
      ctx.WriteTemp(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kMove: {
      assert(instr.operands.size() == 1);
      assert(instr.result.has_value());

      const auto value = ctx.GetTemp(instr.operands[0].AsTempRef());
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in memory handler");
  }
}

}  // namespace lyra::interpreter
