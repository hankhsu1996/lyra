#include "lyra/lowering/mir_to_llvm/layout.hpp"

#include <algorithm>
#include <format>
#include <unordered_set>

#include "llvm/IR/DerivedTypes.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Get the LLVM storage type for an integral type, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  return llvm::Type::getIntNTy(ctx, bit_width);
}

// Get the LLVM type for a TypeId - exhaustive switch for fail-fast on
// unsupported types
auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral:
      return GetLlvmStorageType(ctx, type.AsIntegral().bit_width);

    case TypeKind::kReal:
      return llvm::Type::getDoubleTy(ctx);

    case TypeKind::kString:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      auto width = PackedBitWidth(type, types);
      return GetLlvmStorageType(ctx, width);
    }

    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      llvm::Type* elem = GetLlvmTypeForTypeId(ctx, info.element_type, types);
      return llvm::ArrayType::get(elem, info.range.Size());
    }

    case TypeKind::kVoid:
    case TypeKind::kShortReal:
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedUnion:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      throw common::InternalError(
          "GetLlvmTypeForTypeId",
          std::format(
              "unsupported type kind: {}", static_cast<int>(type.Kind())));
  }

  // Unreachable - all cases handled above
  throw common::InternalError("GetLlvmTypeForTypeId", "unreachable");
}

// Collect a PlaceId from an Operand if it's a use
void CollectPlaceFromOperand(
    const mir::Operand& operand,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  if (operand.kind == mir::Operand::Kind::kUse) {
    places.insert(std::get<mir::PlaceId>(operand.payload));
  }
}

// Collect PlaceIds from an Rvalue
void CollectPlacesFromRvalue(
    const mir::Rvalue& rvalue,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  for (const auto& operand : rvalue.operands) {
    CollectPlaceFromOperand(operand, places);
  }

  if (const auto* guarded_use =
          std::get_if<mir::GuardedUseRvalueInfo>(&rvalue.info)) {
    places.insert(guarded_use->place);
  }

  if (const auto* builtin_call =
          std::get_if<mir::BuiltinCallRvalueInfo>(&rvalue.info)) {
    if (builtin_call->receiver) {
      places.insert(*builtin_call->receiver);
    }
  }
}

// Collect PlaceIds from an EffectOp
void CollectPlacesFromEffectOp(
    const mir::EffectOp& effect,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  std::visit(
      Overloaded{
          [&](const mir::DisplayEffect& d) {
            for (const auto& op : d.ops) {
              if (op.value) {
                CollectPlaceFromOperand(*op.value, places);
              }
            }
          },
          [&](const mir::SeverityEffect& s) {
            for (const auto& arg : s.args) {
              CollectPlaceFromOperand(arg, places);
            }
          },
      },
      effect);
}

// Collect all PlaceIds referenced in a process
auto CollectProcessPlaces(const mir::Process& process)
    -> std::unordered_set<mir::PlaceId, PlaceIdHash> {
  std::unordered_set<mir::PlaceId, PlaceIdHash> places;

  for (const auto& block : process.blocks) {
    for (const auto& instr : block.instructions) {
      std::visit(
          Overloaded{
              [&](const mir::Assign& a) {
                places.insert(a.target);
                CollectPlaceFromOperand(a.source, places);
              },
              [&](const mir::Compute& c) {
                places.insert(c.target);
                CollectPlacesFromRvalue(c.value, places);
              },
              [&](const mir::GuardedAssign& g) {
                places.insert(g.target);
                CollectPlaceFromOperand(g.source, places);
                CollectPlaceFromOperand(g.validity, places);
              },
              [&](const mir::Effect& e) {
                CollectPlacesFromEffectOp(e.op, places);
              },
          },
          instr.data);
    }

    std::visit(
        Overloaded{
            [&](const mir::Branch& b) { places.insert(b.condition); },
            [](const auto&) {},
        },
        block.terminator.data);
  }

  return places;
}

// Collect kLocal/kTemp places for a specific process, sorted by PlaceId
auto CollectFramePlaces(const mir::Process& process, const mir::Arena& arena)
    -> std::vector<mir::PlaceId> {
  auto all_places = CollectProcessPlaces(process);

  std::vector<mir::PlaceId> result;
  for (mir::PlaceId place_id : all_places) {
    const auto& place = arena[place_id];
    if (place.root.kind != mir::PlaceRoot::Kind::kDesign) {
      result.push_back(place_id);
    }
  }

  // Sort by PlaceId value for deterministic ordering
  std::ranges::sort(
      result, [](mir::PlaceId a, mir::PlaceId b) { return a.value < b.value; });
  return result;
}

// Build SuspendRecord struct type
auto BuildSuspendRecordType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  return llvm::StructType::create(
      ctx,
      {llvm::Type::getInt8Ty(ctx),    // tag
       llvm::Type::getInt64Ty(ctx),   // delay_ticks
       llvm::Type::getInt32Ty(ctx)},  // resume_block
      "SuspendRecord");
}

// Build ProcessStateHeader struct type: {SuspendRecord, DesignState*}
auto BuildHeaderType(llvm::LLVMContext& ctx, llvm::StructType* suspend_type)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  return llvm::StructType::create(
      ctx, {suspend_type, ptr_ty}, "ProcessStateHeader");
}

// Build DesignLayout from slot info
auto BuildDesignLayout(
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    llvm::LLVMContext& ctx) -> DesignLayout {
  DesignLayout layout;

  std::vector<llvm::Type*> field_types;

  for (size_t i = 0; i < slots.size(); ++i) {
    const auto& slot = slots[i];
    layout.slots.push_back(slot.slot_id);
    layout.slot_to_field[slot.slot_id] = static_cast<uint32_t>(i);
    // Use actual TypeId for LLVM type derivation (not SlotTypeInfo)
    field_types.push_back(GetLlvmTypeForTypeId(ctx, slot.type_id, types));
  }

  // Empty struct needs sentinel for valid LLVM struct
  if (field_types.empty()) {
    layout.llvm_type = llvm::StructType::create(
        ctx, {llvm::Type::getInt8Ty(ctx)}, "DesignState");
  } else {
    layout.llvm_type =
        llvm::StructType::create(ctx, field_types, "DesignState");
  }

  return layout;
}

// Build FrameLayout for a process
auto BuildFrameLayout(
    const std::vector<mir::PlaceId>& frame_places, const mir::Arena& arena,
    const TypeArena& types, llvm::LLVMContext& ctx, size_t process_index)
    -> FrameLayout {
  FrameLayout layout;

  std::vector<llvm::Type*> field_types;

  for (size_t i = 0; i < frame_places.size(); ++i) {
    mir::PlaceId place_id = frame_places[i];
    layout.places.push_back(place_id);
    layout.place_to_field[place_id] = static_cast<uint32_t>(i);

    const auto& place = arena[place_id];
    field_types.push_back(GetLlvmTypeForTypeId(ctx, place.root.type, types));
  }

  std::string name = std::format("ProcessFrame{}", process_index);

  // Empty struct needs sentinel for valid LLVM struct
  if (field_types.empty()) {
    layout.llvm_type =
        llvm::StructType::create(ctx, {llvm::Type::getInt8Ty(ctx)}, name);
  } else {
    layout.llvm_type = llvm::StructType::create(ctx, field_types, name);
  }

  return layout;
}

// Build ProcessStateN type: {ProcessStateHeader, ProcessFrameN}
auto BuildProcessStateType(
    llvm::LLVMContext& ctx, llvm::StructType* header_type,
    llvm::StructType* frame_type, size_t process_index) -> llvm::StructType* {
  std::string name = std::format("ProcessState{}", process_index);
  return llvm::StructType::create(ctx, {header_type, frame_type}, name);
}

}  // namespace

auto BuildLayout(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const std::vector<SlotInfo>& slots, llvm::LLVMContext& ctx) -> Layout {
  Layout layout;

  // Build runtime types
  layout.suspend_record_type = BuildSuspendRecordType(ctx);
  layout.header_type = BuildHeaderType(ctx, layout.suspend_record_type);

  // Build design layout (use actual TypeIds for type derivation)
  layout.design = BuildDesignLayout(slots, types, ctx);

  // Collect all initial processes (single source of truth for ordering)
  for (const auto& element : design.elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    for (mir::ProcessId proc_id : mir_module.processes) {
      const auto& process = arena[proc_id];
      if (process.kind == mir::ProcessKind::kOnce) {
        layout.process_ids.push_back(proc_id);
      }
    }
  }

  // Build process layouts
  layout.processes.reserve(layout.process_ids.size());

  for (size_t i = 0; i < layout.process_ids.size(); ++i) {
    const auto& process = arena[layout.process_ids[i]];

    ProcessLayout proc_layout;
    proc_layout.process_index = i;

    // Collect frame places
    auto frame_places = CollectFramePlaces(process, arena);

    // Build frame layout
    proc_layout.frame = BuildFrameLayout(frame_places, arena, types, ctx, i);

    // Build process state type
    proc_layout.state_type = BuildProcessStateType(
        ctx, layout.header_type, proc_layout.frame.llvm_type, i);

    layout.processes.push_back(std::move(proc_layout));
  }

  return layout;
}

}  // namespace lyra::lowering::mir_to_llvm
