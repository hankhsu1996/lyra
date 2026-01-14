#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

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

// Check if an expression can be converted to a pointer chain
// (i.e., it's an identifier or a chain of element/field selects leading to
// identifier). Packed array element access is NOT addressable - it's a
// bit slice within a bitvector, not a distinct storage location.
auto IsAddressable(const mir::Expression& expr) -> bool {
  if (expr.kind == mir::Expression::Kind::kIdentifier) {
    return true;
  }
  if (expr.kind == mir::Expression::Kind::kElementSelect) {
    const auto& elem_select = mir::As<mir::ElementSelectExpression>(expr);
    // Packed array element access is not addressable (it's a bit slice)
    if (elem_select.value->type.IsBitvector()) {
      return false;
    }
    return IsAddressable(*elem_select.value);
  }
  if (expr.kind == mir::Expression::Kind::kMemberAccess) {
    const auto& member = mir::As<mir::MemberAccessExpression>(expr);
    // Only unpacked struct/union field access is addressable
    if (member.value->type.IsUnpackedStruct() ||
        member.value->type.IsUnpackedUnion()) {
      return IsAddressable(*member.value);
    }
  }
  return false;
}

// Build a pointer chain to an array element.
// For arr[i]: ResolveVar(arr) -> ResolveIndex(i)
// For arr[i][j]: ResolveVar(arr) -> ResolveIndex(i) -> ResolveIndex(j)
// Note: lower_bound adjustment happens at runtime in ReadPointer/WritePointer.
// The index stored in IndexPointer is the raw SV index.
auto LowerToElementPointer(
    const mir::Expression& expr, TempRef index, const Type& element_type,
    LirBuilder& builder) -> TempRef {
  if (expr.kind == mir::Expression::Kind::kIdentifier) {
    // Base case: variable reference
    const auto& id = mir::As<mir::IdentifierExpression>(expr);
    const auto* arr_pointee = builder.GetContext()->InternType(id.type);
    auto ptr_arr =
        builder.AllocateTemp("ptr", common::Type::Pointer(arr_pointee));
    builder.AddInstruction(Instruction::ResolveVar(ptr_arr, id.symbol));

    // ResolveIndex to get pointer to element (stores raw SV index)
    const auto* elem_pointee = builder.GetContext()->InternType(element_type);
    auto ptr_elem =
        builder.AllocateTemp("ptr_elem", common::Type::Pointer(elem_pointee));
    builder.AddInstruction(Instruction::ResolveIndex(ptr_elem, ptr_arr, index));
    return ptr_elem;
  }

  if (expr.kind == mir::Expression::Kind::kElementSelect) {
    // Recursive case: nested array access (arr[i][j])
    const auto& elem_select = mir::As<mir::ElementSelectExpression>(expr);

    // Recursively get pointer to the intermediate element
    auto outer_index = LowerExpression(*elem_select.selector, builder);
    auto ptr_outer = LowerToElementPointer(
        *elem_select.value, outer_index, elem_select.type, builder);

    // ResolveIndex from the intermediate pointer
    const auto* elem_pointee = builder.GetContext()->InternType(element_type);
    auto ptr_elem =
        builder.AllocateTemp("ptr_elem", common::Type::Pointer(elem_pointee));
    builder.AddInstruction(
        Instruction::ResolveIndex(ptr_elem, ptr_outer, index));
    return ptr_elem;
  }

  // Should not reach here - IsAddressable should have returned false
  throw common::InternalError(
      "lowering", "LowerToElementPointer called on non-addressable expression");
}

// Build a pointer to an addressable expression (variable, array element, or
// struct field). Returns a Pointer<T> to the expression's storage location.
auto LowerToPointer(const mir::Expression& expr, LirBuilder& builder)
    -> TempRef {
  if (expr.kind == mir::Expression::Kind::kIdentifier) {
    // Base case: variable reference
    const auto& id = mir::As<mir::IdentifierExpression>(expr);
    const auto* pointee = builder.GetContext()->InternType(id.type);
    auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
    builder.AddInstruction(Instruction::ResolveVar(ptr, id.symbol));
    return ptr;
  }

  if (expr.kind == mir::Expression::Kind::kElementSelect) {
    // Array element: get pointer to array, then resolve index
    const auto& elem_select = mir::As<mir::ElementSelectExpression>(expr);
    auto base_ptr = LowerToPointer(*elem_select.value, builder);
    auto index = LowerExpression(*elem_select.selector, builder);

    const auto* elem_pointee =
        builder.GetContext()->InternType(elem_select.type);
    auto elem_ptr =
        builder.AllocateTemp("ptr_elem", common::Type::Pointer(elem_pointee));
    builder.AddInstruction(
        Instruction::ResolveIndex(elem_ptr, base_ptr, index));
    return elem_ptr;
  }

  if (expr.kind == mir::Expression::Kind::kMemberAccess) {
    // Struct field: get pointer to struct, then resolve field
    const auto& member = mir::As<mir::MemberAccessExpression>(expr);
    auto base_ptr = LowerToPointer(*member.value, builder);

    // For union: ALWAYS use index 0 (shared storage)
    size_t field_id =
        member.value->type.IsUnpackedUnion() ? 0 : member.bit_offset;

    const auto* field_pointee = builder.GetContext()->InternType(member.type);
    auto field_ptr =
        builder.AllocateTemp("ptr_field", common::Type::Pointer(field_pointee));
    builder.AddInstruction(
        Instruction::ResolveField(field_ptr, base_ptr, field_id));
    return field_ptr;
  }

  throw common::InternalError(
      "lowering", "LowerToPointer called on non-addressable expression");
}

auto LowerElementSelectExpression(
    const mir::ElementSelectExpression& select, LirBuilder& builder)
    -> lir::TempRef {
  assert(select.value);
  assert(select.selector);

  auto index = LowerExpression(*select.selector, builder);
  auto result = builder.AllocateTemp("elem", select.type);

  // Check if this is bitvector type (value) or unpacked array (variable)
  if (select.value->type.IsBitvector()) {
    // Packed vector element access
    int32_t lower = select.value->type.GetElementLower();
    auto adjusted_index = AdjustForNonZeroLower(index, lower, builder);

    // Compute bit_offset = adjusted_index * element_width
    size_t element_width = select.type.GetBitWidth();
    auto bit_offset = builder.AllocateTemp("bit_offset", Type::Int());
    auto width_constant = builder.InternConstant(
        Constant::Int(static_cast<int32_t>(element_width)));
    auto width_temp = builder.AllocateTemp("width", Type::Int());
    builder.AddInstruction(Instruction::Constant(width_temp, width_constant));
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryMultiply, bit_offset,
            {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

    if (IsAddressable(*select.value)) {
      // SliceRef approach: ResolveVar -> ResolveSlice -> LoadSlice
      auto ptr = LowerToPointer(*select.value, builder);
      const auto* slice_pointee = builder.GetContext()->InternType(select.type);
      auto slice_ref =
          builder.AllocateTemp("slice_ref", Type::SliceRef(slice_pointee));
      builder.AddInstruction(
          Instruction::ResolveSlice(slice_ref, ptr, bit_offset, width_temp));
      builder.AddInstruction(Instruction::LoadSlice(result, slice_ref));
    } else {
      // ExtractBits approach for non-addressable expressions
      auto value = LowerExpression(*select.value, builder);
      builder.AddInstruction(
          Instruction::ExtractBits(result, value, bit_offset, select.type));
    }
    return result;
  }

  // Unpacked array/queue element access
  const auto& value_type = select.value->type;

  // Use pointer-based approach if addressable, otherwise fall back to intrinsic
  if (IsAddressable(*select.value)) {
    // Lower bound adjustment happens at runtime in ReadPointer
    auto ptr_elem =
        LowerToElementPointer(*select.value, index, select.type, builder);
    builder.AddInstruction(Instruction::Load(result, ptr_elem));
  } else {
    // Non-addressable expression (e.g., function return): use intrinsic
    // Compute lower_bound for unpacked arrays (queues always have 0)
    int32_t lower_bound = 0;
    if (value_type.kind == Type::Kind::kUnpackedArray) {
      const auto& array_data =
          std::get<common::UnpackedArrayData>(value_type.data);
      lower_bound = array_data.lower_bound;
    }
    auto receiver = LowerExpression(*select.value, builder);
    void* intrinsic_fn =
        interpreter::ResolveIntrinsicIndexRead(value_type.kind);
    auto instr = Instruction::IntrinsicCall(
        intrinsic_fn, receiver, {index}, result, select.type);
    instr.lower_bound = lower_bound;
    builder.AddInstruction(std::move(instr));
  }

  return result;
}

auto LowerRangeSelectExpression(
    const mir::RangeSelectExpression& range, LirBuilder& builder)
    -> lir::TempRef {
  assert(range.value);

  // Compute LSB: for [7:4], LSB is 4
  int32_t lsb = std::min(range.left, range.right);

  // Adjust for non-zero-based ranges (e.g., bit [63:32])
  // Packed structs return 0 from GetElementLower() (always 0-based)
  if (range.value->type.IsBitvector()) {
    lsb -= range.value->type.GetElementLower();
  }

  // Create constants for LSB and width
  auto lsb_temp = builder.AllocateTemp("lsb", Type::Int());
  auto lsb_constant = builder.InternConstant(Constant::Int(lsb));
  builder.AddInstruction(Instruction::Constant(lsb_temp, lsb_constant));

  size_t width = range.type.GetBitWidth();
  auto width_temp = builder.AllocateTemp("width", Type::Int());
  auto width_constant =
      builder.InternConstant(Constant::Int(static_cast<int32_t>(width)));
  builder.AddInstruction(Instruction::Constant(width_temp, width_constant));

  auto result = builder.AllocateTemp("slice", range.type);

  if (IsAddressable(*range.value)) {
    // SliceRef approach: ResolveVar -> ResolveSlice -> LoadSlice
    auto ptr = LowerToPointer(*range.value, builder);
    const auto* slice_pointee = builder.GetContext()->InternType(range.type);
    auto slice_ref =
        builder.AllocateTemp("slice_ref", Type::SliceRef(slice_pointee));
    builder.AddInstruction(
        Instruction::ResolveSlice(slice_ref, ptr, lsb_temp, width_temp));
    builder.AddInstruction(Instruction::LoadSlice(result, slice_ref));
  } else {
    // ExtractBits approach for non-addressable expressions
    auto value = LowerExpression(*range.value, builder);
    builder.AddInstruction(
        Instruction::ExtractBits(result, value, lsb_temp, range.type));
  }
  return result;
}

auto LowerIndexedRangeSelectExpression(
    const mir::IndexedRangeSelectExpression& indexed, LirBuilder& builder)
    -> lir::TempRef {
  assert(indexed.value);
  assert(indexed.start);

  auto start = LowerExpression(*indexed.start, builder);

  TempRef lsb_temp;
  if (indexed.is_ascending) {
    // a[i+:4]: lsb = i (start index is the LSB)
    lsb_temp = start;
  } else {
    // a[i-:4]: lsb = i - width + 1
    auto offset_temp = builder.AllocateTemp("offset", Type::Int());
    auto offset_constant =
        builder.InternConstant(Constant::Int(indexed.width - 1));
    builder.AddInstruction(Instruction::Constant(offset_temp, offset_constant));

    lsb_temp = builder.AllocateTemp("lsb", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinarySubtract, lsb_temp,
            {Operand::Temp(start), Operand::Temp(offset_temp)}));
  }

  // Adjust for non-zero-based ranges if needed
  int32_t lower = indexed.value->type.GetElementLower();
  lsb_temp = AdjustForNonZeroLower(lsb_temp, lower, builder);

  // Create width constant
  auto width_temp = builder.AllocateTemp("width", Type::Int());
  auto width_constant = builder.InternConstant(
      Constant::Int(static_cast<int32_t>(indexed.width)));
  builder.AddInstruction(Instruction::Constant(width_temp, width_constant));

  auto result = builder.AllocateTemp("slice", indexed.type);

  if (IsAddressable(*indexed.value)) {
    // SliceRef approach: ResolveVar -> ResolveSlice -> LoadSlice
    auto ptr = LowerToPointer(*indexed.value, builder);
    const auto* slice_pointee = builder.GetContext()->InternType(indexed.type);
    auto slice_ref =
        builder.AllocateTemp("slice_ref", Type::SliceRef(slice_pointee));
    builder.AddInstruction(
        Instruction::ResolveSlice(slice_ref, ptr, lsb_temp, width_temp));
    builder.AddInstruction(Instruction::LoadSlice(result, slice_ref));
  } else {
    // ExtractBits approach for non-addressable expressions
    auto value = LowerExpression(*indexed.value, builder);
    builder.AddInstruction(
        Instruction::ExtractBits(result, value, lsb_temp, indexed.type));
  }
  return result;
}

auto LowerMemberAccessExpression(
    const mir::MemberAccessExpression& member, LirBuilder& builder)
    -> lir::TempRef {
  assert(member.value);

  // Check if this is an unpacked struct/union access
  if (member.value->type.IsUnpackedStruct() ||
      member.value->type.IsUnpackedUnion()) {
    auto result = builder.AllocateTemp("field", member.type);

    // For struct: bit_offset is reused as field_index
    // For union: ALWAYS use index 0 (shared storage)
    size_t field_id =
        member.value->type.IsUnpackedUnion() ? 0 : member.bit_offset;

    // Use pointer-based approach if receiver is addressable
    if (IsAddressable(*member.value)) {
      auto base_ptr = LowerToPointer(*member.value, builder);
      const auto* field_pointee = builder.GetContext()->InternType(member.type);
      auto field_ptr = builder.AllocateTemp(
          "ptr_field", common::Type::Pointer(field_pointee));
      builder.AddInstruction(
          Instruction::ResolveField(field_ptr, base_ptr, field_id));
      builder.AddInstruction(Instruction::Load(result, field_ptr));
      return result;
    }

    // Non-addressable (e.g., function return): fall back to intrinsic
    auto receiver = LowerExpression(*member.value, builder);
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_constant =
        builder.InternConstant(Constant::Int(static_cast<int32_t>(field_id)));
    builder.AddInstruction(Instruction::Constant(index_temp, index_constant));

    void* intrinsic_fn =
        interpreter::ResolveIntrinsicIndexRead(member.value->type.kind);
    builder.AddInstruction(
        Instruction::IntrinsicCall(
            intrinsic_fn, receiver, {index_temp}, result, member.type));
    return result;
  }

  // Packed struct: use bit extraction
  // Create constants for bit offset (LSB position) and width
  auto offset_temp = builder.AllocateTemp("offset", Type::Int());
  auto offset_constant = builder.InternConstant(
      Constant::Int(static_cast<int32_t>(member.bit_offset)));
  builder.AddInstruction(Instruction::Constant(offset_temp, offset_constant));

  size_t width = member.type.GetBitWidth();
  auto width_temp = builder.AllocateTemp("width", Type::Int());
  auto width_constant =
      builder.InternConstant(Constant::Int(static_cast<int32_t>(width)));
  builder.AddInstruction(Instruction::Constant(width_temp, width_constant));

  auto result = builder.AllocateTemp("field", member.type);

  if (IsAddressable(*member.value)) {
    // SliceRef approach: ResolveVar -> ResolveSlice -> LoadSlice
    auto ptr = LowerToPointer(*member.value, builder);
    const auto* slice_pointee = builder.GetContext()->InternType(member.type);
    auto slice_ref =
        builder.AllocateTemp("slice_ref", Type::SliceRef(slice_pointee));
    builder.AddInstruction(
        Instruction::ResolveSlice(slice_ref, ptr, offset_temp, width_temp));
    builder.AddInstruction(Instruction::LoadSlice(result, slice_ref));
  } else {
    // ExtractBits approach for non-addressable expressions
    auto value = LowerExpression(*member.value, builder);
    builder.AddInstruction(
        Instruction::ExtractBits(result, value, offset_temp, member.type));
  }
  return result;
}

auto LowerHierarchicalReferenceExpression(
    const mir::HierarchicalReferenceExpression& hier_ref, LirBuilder& builder)
    -> lir::TempRef {
  // Hierarchical reference uses target_symbol directly (flat storage model)
  const auto* pointee = builder.GetContext()->InternType(hier_ref.type);
  auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
  builder.AddInstruction(Instruction::ResolveVar(ptr, hier_ref.target_symbol));

  auto result = builder.AllocateTemp("hier", hier_ref.type);
  builder.AddInstruction(Instruction::Load(result, ptr));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
