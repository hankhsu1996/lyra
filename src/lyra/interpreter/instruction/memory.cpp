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
#include "lyra/interpreter/address.hpp"
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

      // Create an address pointing to the variable
      auto addr = Address::Var(*instr.symbol);
      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Pointer(ptr_type, std::move(addr)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoad: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(ptr_value.IsPointer());

      RuntimeValue loaded = ctx.ResolveForRead(ptr_value.AsAddress());
      ctx.WriteTemp(instr.result.value(), std::move(loaded));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStore: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.ResolveForWrite(ptr_value.AsAddress(), value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreNBA: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.ResolveForWrite(ptr_value.AsAddress(), value, true);
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

      // Build address by appending index to base address path
      auto elem_addr =
          base_ptr_value.AsAddress().WithIndex(static_cast<uint32_t>(index));

      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Pointer(ptr_type, std::move(elem_addr)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveField: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& base_ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(base_ptr_value.IsPointer());

      // field_id is stored in lower_bound field
      auto field_id = static_cast<uint32_t>(instr.lower_bound);

      // Build address by appending field to base address path
      auto field_addr = base_ptr_value.AsAddress().WithField(field_id);

      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Pointer(ptr_type, std::move(field_addr)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadSlice: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 3);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(ptr_value.IsPointer());

      const auto& offset_value = ctx.GetTemp(instr.temp_operands[1]);
      const auto& width_value = ctx.GetTemp(instr.temp_operands[2]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      assert(!width_value.IsWide() && "bit width cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());
      auto width = static_cast<size_t>(width_value.AsNarrow().AsInt64());

      // Read the parent storage
      RuntimeValue parent = ctx.ResolveForRead(ptr_value.AsAddress());

      // Extract bits from the parent value
      const auto& result_type = (*instr.result)->type;
      bool is_signed = result_type.IsSigned();

      RuntimeValue result;
      if (width <= 64) {
        uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
        uint64_t extracted = 0;
        if (parent.IsWide()) {
          auto shifted = parent.AsWideBit().ShiftRightLogical(bit_offset);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (parent.AsNarrow().AsUInt64() >> bit_offset) & mask;
        }
        result = is_signed ? RuntimeValue::IntegralSigned(
                                 static_cast<int64_t>(extracted), width)
                           : RuntimeValue::IntegralUnsigned(extracted, width);
      } else {
        auto wide_value =
            parent.IsWide()
                ? parent.AsWideBit()
                : common::WideBit::FromUInt64(parent.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(bit_offset, width);
        result =
            RuntimeValue::IntegralWide(std::move(extracted), width, is_signed);
      }

      ctx.WriteTemp(instr.result.value(), std::move(result));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreSlice: {
      assert(instr.temp_operands.size() == 4);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      const auto& offset_value = ctx.GetTemp(instr.temp_operands[2]);
      const auto& width_value = ctx.GetTemp(instr.temp_operands[3]);
      assert(ptr_value.IsPointer());
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      assert(!width_value.IsWide() && "bit width cannot be wide");

      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());
      auto width = static_cast<size_t>(width_value.AsNarrow().AsInt64());

      ctx.ResolveForWriteSlice(
          ptr_value.AsAddress(), value, bit_offset, width, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kAllocate: {
      assert(instr.result.has_value());
      const auto& result_type = (*instr.result)->type;

      // Pointer<T> result: allocate T in anonymous storage and return pointer
      if (result_type.IsPointer()) {
        const auto& alloc_type = result_type.GetPointeeType();
        RuntimeValue storage_value;

        if (instr.operands.empty()) {
          // Fixed-size allocation
          storage_value = RuntimeValue::DefaultValueForType(alloc_type);
        } else {
          // Dynamic allocation with size (and optional init)
          auto size_val = ctx.GetOperandValue(instr.operands[0]);
          auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

          const auto& dyn_data =
              std::get<common::DynamicArrayData>(alloc_type.data);
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
                elements.push_back(
                    RuntimeValue::DefaultValueForType(elem_type));
              }
            }
          } else {
            for (size_t i = 0; i < size; ++i) {
              elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
            }
          }

          storage_value = RuntimeValue::Array(alloc_type, std::move(elements));
        }

        // Allocate in anonymous storage and return pointer
        auto alloc_id = ctx.AllocateAnonymous(std::move(storage_value));
        auto addr = Address::Alloc(alloc_id);
        ctx.WriteTemp(
            instr.result.value(),
            RuntimeValue::Pointer(result_type, std::move(addr)));
        return InstructionResult::Continue();
      }

      // Non-pointer result: direct value allocation (used for queues)
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
