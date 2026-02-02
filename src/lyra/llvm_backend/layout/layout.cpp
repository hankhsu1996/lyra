#include "lyra/llvm_backend/layout/layout.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

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

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType* {
  auto* elem = GetLlvmStorageType(ctx, bit_width);
  return llvm::StructType::get(ctx, {elem, elem});
}

namespace {

// Forward declaration for recursive call in BuildUnpackedStructType
auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type*;

// Compute allocation size for a type in bytes (for union storage calculation)
auto ComputeAllocSize(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types) -> uint32_t;

// Compute alignment for a type in bytes (for union storage calculation)
auto ComputeAlignment(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> uint32_t {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bits = type.AsIntegral().bit_width;
      // Round to power-of-2 storage, alignment = storage size (up to 8 bytes)
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      return 8;  // i64 and larger use 8-byte alignment
    }
    case TypeKind::kReal:
      return 8;  // double
    case TypeKind::kShortReal:
      return 4;  // float
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bits = PackedBitWidth(type, types);
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      return 8;
    }
    case TypeKind::kUnpackedStruct: {
      // Struct alignment = max alignment of fields
      const auto& info = type.AsUnpackedStruct();
      uint32_t max_align = 1;
      for (const auto& field : info.fields) {
        max_align =
            std::max(max_align, ComputeAlignment(ctx, field.type, types));
      }
      return max_align;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return ComputeAlignment(ctx, info.element_type, types);
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      uint32_t max_align = 1;
      for (const auto& member : info.members) {
        max_align =
            std::max(max_align, ComputeAlignment(ctx, member.type, types));
      }
      return max_align;
    }
    default:
      return 1;
  }
}

auto ComputeAllocSize(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> uint32_t {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bits = type.AsIntegral().bit_width;
      // Round to power-of-2 storage
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      if (bits <= 64) {
        return 8;
      }
      // Larger integrals: round up to bytes
      return (bits + 7) / 8;
    }
    case TypeKind::kReal:
      return 8;  // double
    case TypeKind::kShortReal:
      return 4;  // float
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bits = PackedBitWidth(type, types);
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      if (bits <= 64) {
        return 8;
      }
      return (bits + 7) / 8;
    }
    case TypeKind::kUnpackedStruct: {
      // Sum of field sizes with alignment padding
      const auto& info = type.AsUnpackedStruct();
      uint32_t offset = 0;
      uint32_t max_align = 1;
      for (const auto& field : info.fields) {
        uint32_t field_align = ComputeAlignment(ctx, field.type, types);
        uint32_t field_size = ComputeAllocSize(ctx, field.type, types);
        max_align = std::max(max_align, field_align);
        // Align offset
        offset = (offset + field_align - 1) / field_align * field_align;
        offset += field_size;
      }
      // Pad to struct alignment
      return (offset + max_align - 1) / max_align * max_align;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      uint32_t elem_size = ComputeAllocSize(ctx, info.element_type, types);
      return elem_size * info.range.Size();
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      uint32_t max_size = 0;
      uint32_t max_align = 1;
      for (const auto& member : info.members) {
        max_size =
            std::max(max_size, ComputeAllocSize(ctx, member.type, types));
        max_align =
            std::max(max_align, ComputeAlignment(ctx, member.type, types));
      }
      // Round up to alignment
      return (max_size + max_align - 1) / max_align * max_align;
    }
    default:
      throw common::InternalError(
          "ComputeAllocSize",
          std::format(
              "unsupported type kind: {}", static_cast<int>(type.Kind())));
  }
}

// Build LLVM byte array type for an unpacked union
auto BuildUnpackedUnionType(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  uint32_t size = ComputeAllocSize(ctx, type_id, types);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  return llvm::ArrayType::get(i8_ty, size);
}

// Build LLVM struct type for an unpacked struct TypeId
auto BuildUnpackedStructType(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types) -> llvm::Type* {
  std::vector<llvm::Type*> field_types;
  field_types.reserve(info.fields.size());
  for (const auto& field : info.fields) {
    field_types.push_back(GetLlvmTypeForTypeId(ctx, field.type, types));
  }
  return llvm::StructType::get(ctx, field_types);
}

// Get the LLVM type for a TypeId - exhaustive switch for fail-fast on
// unsupported types
auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      if (type.AsIntegral().is_four_state) {
        return GetFourStateStructType(ctx, bit_width);
      }
      return GetLlvmStorageType(ctx, bit_width);
    }

    case TypeKind::kReal:
      return llvm::Type::getDoubleTy(ctx);

    case TypeKind::kString:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      auto width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        return GetFourStateStructType(ctx, width);
      }
      return GetLlvmStorageType(ctx, width);
    }

    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      llvm::Type* elem = GetLlvmTypeForTypeId(ctx, info.element_type, types);
      return llvm::ArrayType::get(elem, info.range.Size());
    }

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kShortReal:
      return llvm::Type::getFloatTy(ctx);

    case TypeKind::kUnpackedStruct:
      return BuildUnpackedStructType(ctx, type.AsUnpackedStruct(), types);

    case TypeKind::kUnpackedUnion:
      return BuildUnpackedUnionType(ctx, type_id, types);

    case TypeKind::kVoid:
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

// Forward declarations for mutual recursion
void CollectPlacesFromRvalue(
    const mir::Rvalue& rvalue,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places);

// Collect PlaceIds from a RightHandSide
void CollectPlacesFromRhs(
    const mir::RightHandSide& rhs,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  std::visit(
      common::Overloaded{
          [&](const mir::Operand& op) { CollectPlaceFromOperand(op, places); },
          [&](const mir::Rvalue& rv) { CollectPlacesFromRvalue(rv, places); },
      },
      rhs);
}

// Collect PlaceIds from an Rvalue (operands + RvalueInfo).
// Uses exhaustive std::visit to ensure new RvalueInfo types with embedded
// PlaceIds cause compilation errors until handled.
void CollectPlacesFromRvalue(
    const mir::Rvalue& rvalue,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  for (const auto& operand : rvalue.operands) {
    CollectPlaceFromOperand(operand, places);
  }

  std::visit(
      [&](const auto& info) {
        using T = std::decay_t<decltype(info)>;

        if constexpr (std::is_same_v<T, mir::GuardedUseRvalueInfo>) {
          places.insert(info.place);
        } else if constexpr (std::is_same_v<T, mir::BuiltinCallRvalueInfo>) {
          if (info.receiver) {
            places.insert(*info.receiver);
          }
        } else if constexpr (std::is_same_v<T, mir::TestPlusargsRvalueInfo>) {
          // Test plusargs uses info.query, not Rvalue::operands
          CollectPlaceFromOperand(info.query.operand, places);
        } else if constexpr (std::is_same_v<T, mir::FopenRvalueInfo>) {
          // Fopen uses info.filename/mode, not Rvalue::operands
          CollectPlaceFromOperand(info.filename.operand, places);
          if (info.mode) {
            CollectPlaceFromOperand(info.mode->operand, places);
          }
        } else if constexpr (std::is_same_v<T, mir::SFormatRvalueInfo>) {
          for (const auto& fop : info.ops) {
            if (fop.value) {
              CollectPlaceFromOperand(*fop.value, places);
            }
          }
        } else if constexpr (
            std::is_same_v<T, mir::UnaryRvalueInfo> ||
            std::is_same_v<T, mir::BinaryRvalueInfo> ||
            std::is_same_v<T, mir::CastRvalueInfo> ||
            std::is_same_v<T, mir::BitCastRvalueInfo> ||
            std::is_same_v<T, mir::AggregateRvalueInfo> ||
            std::is_same_v<T, mir::IndexValidityRvalueInfo> ||
            std::is_same_v<T, mir::ConcatRvalueInfo> ||
            std::is_same_v<T, mir::ReplicateRvalueInfo> ||
            std::is_same_v<T, mir::RuntimeQueryRvalueInfo> ||
            std::is_same_v<T, mir::MathCallRvalueInfo> ||
            std::is_same_v<T, mir::SystemTfRvalueInfo>) {
          // These RvalueInfo types have no embedded PlaceIds or Operands
          // beyond what's in Rvalue::operands (already collected above)
        } else {
          static_assert(
              !sizeof(T),
              "Unhandled RvalueInfo type - if this type has embedded PlaceIds "
              "or Operands, add handling above");
        }
      },
      rvalue.info);
}

// Collect PlaceIds from an EffectOp
void CollectPlacesFromEffectOp(
    const mir::EffectOp& effect,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  std::visit(
      common::Overloaded{
          [&](const mir::DisplayEffect& d) {
            if (d.descriptor) {
              CollectPlaceFromOperand(*d.descriptor, places);
            }
            for (const auto& op : d.ops) {
              if (op.value) {
                CollectPlaceFromOperand(*op.value, places);
              }
            }
          },
          [&](const mir::SeverityEffect& s) {
            for (const auto& op : s.ops) {
              if (op.value.has_value()) {
                CollectPlaceFromOperand(*op.value, places);
              }
            }
          },
          [&](const mir::MemIOEffect& m) {
            places.insert(m.target);
            CollectPlaceFromOperand(m.filename.operand, places);
            if (m.start_addr) {
              CollectPlaceFromOperand(*m.start_addr, places);
            }
            if (m.end_addr) {
              CollectPlaceFromOperand(*m.end_addr, places);
            }
          },
          [&](const mir::TimeFormatEffect&) {
            // TimeFormatEffect has no operands - all constants
          },
          [&](const mir::SystemTfEffect& s) {
            for (const auto& arg : s.args) {
              CollectPlaceFromOperand(arg, places);
            }
          },
          [&](const mir::StrobeEffect&) {
            // StrobeEffect only has FunctionId thunk, no operands
          },
          [&](const mir::MonitorEffect&) {
            // MonitorEffect only has FunctionIds and buffer size, no operands
          },
          [&](const mir::MonitorControlEffect&) {
            // MonitorControlEffect only has enable flag, no operands
          },
          [&](const mir::FillPackedEffect& f) {
            places.insert(f.target);
            CollectPlaceFromOperand(f.fill_value, places);
          },
      },
      effect);
}

// Collect all PlaceIds referenced in a process
auto CollectProcessPlaces(const mir::Process& process)
    -> std::unordered_set<mir::PlaceId, PlaceIdHash> {
  std::unordered_set<mir::PlaceId, PlaceIdHash> places;

  for (const auto& block : process.blocks) {
    for (const auto& instr : block.statements) {
      std::visit(
          common::Overloaded{
              [&](const mir::Assign& a) {
                places.insert(a.dest);
                CollectPlacesFromRhs(a.rhs, places);
              },
              [&](const mir::GuardedAssign& ga) {
                places.insert(ga.dest);
                CollectPlacesFromRhs(ga.rhs, places);
                CollectPlaceFromOperand(ga.guard, places);
              },
              [&](const mir::Effect& e) {
                CollectPlacesFromEffectOp(e.op, places);
              },
              [&](const mir::DeferredAssign& da) {
                places.insert(da.dest);
                CollectPlacesFromRhs(da.rhs, places);
              },
              [&](const mir::Call& call) {
                // Collect return tmp (not dest - dest may be design slot)
                if (call.ret) {
                  places.insert(call.ret->tmp);
                }
                // Collect writeback tmps (not dests)
                for (const auto& wb : call.writebacks) {
                  places.insert(wb.tmp);
                }
                // Collect input args
                for (const auto& arg : call.in_args) {
                  CollectPlaceFromOperand(arg, places);
                }
              },
              [&](const mir::BuiltinCall& bcall) {
                if (bcall.dest) {
                  places.insert(*bcall.dest);
                }
                places.insert(bcall.receiver);
                for (const auto& arg : bcall.args) {
                  CollectPlaceFromOperand(arg, places);
                }
              },
          },
          instr.data);
    }

    std::visit(
        common::Overloaded{
            [&](const mir::Branch& b) { places.insert(b.condition); },
            [](const auto&) {},
        },
        block.terminator.data);
  }

  return places;
}

struct RootInfo {
  PlaceRootKey key;
  TypeId type;
};

// Collect unique roots for frame layout, de-duplicating projected places
auto CollectFrameRoots(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types) -> std::vector<RootInfo> {
  auto all_places = CollectProcessPlaces(process);

  std::unordered_map<PlaceRootKey, TypeId, PlaceRootKeyHash> seen;
  for (mir::PlaceId place_id : all_places) {
    const auto& place = arena[place_id];
    if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
      continue;
    }
    // Void-typed places (e.g. targets of .delete()) never need storage
    if (types[place.root.type].Kind() == TypeKind::kVoid) {
      continue;
    }
    PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
    seen.emplace(key, place.root.type);
  }

  // Sort by (kind, id) for deterministic ordering
  std::vector<RootInfo> result;
  result.reserve(seen.size());
  for (const auto& [key, type] : seen) {
    result.push_back(RootInfo{.key = key, .type = type});
  }
  std::ranges::sort(result, [](const RootInfo& a, const RootInfo& b) {
    if (a.key.kind != b.key.kind) {
      return a.key.kind < b.key.kind;
    }
    return a.key.id < b.key.id;
  });
  return result;
}

// Build SuspendRecord as opaque blob - suspend helpers own the layout.
auto BuildSuspendRecordType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  static_assert(
      alignof(lyra::runtime::SuspendRecord) >= 8,
      "SuspendRecord must be at least 8-byte aligned for i64 blob");
  static_assert(
      sizeof(lyra::runtime::SuspendRecord) % 8 == 0,
      "SuspendRecord size must be multiple of 8 for i64 blob");
  auto* payload = llvm::ArrayType::get(
      llvm::Type::getInt64Ty(ctx), sizeof(lyra::runtime::SuspendRecord) / 8);
  return llvm::StructType::create(ctx, {payload}, "SuspendRecord");
}

// Build ProcessStateHeader: {SuspendRecord, DesignState*, Engine*}
auto BuildHeaderType(llvm::LLVMContext& ctx, llvm::StructType* suspend_type)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  return llvm::StructType::create(
      ctx, {suspend_type, ptr_ty, ptr_ty}, "ProcessStateHeader");
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

// Build FrameLayout from de-duplicated roots
auto BuildFrameLayout(
    const std::vector<RootInfo>& roots, const TypeArena& types,
    llvm::LLVMContext& ctx, size_t process_index) -> FrameLayout {
  FrameLayout layout;
  std::vector<llvm::Type*> field_types;

  for (size_t i = 0; i < roots.size(); ++i) {
    const auto& root = roots[i];
    layout.root_types.push_back(root.type);
    layout.root_to_field[root.key] = static_cast<uint32_t>(i);
    field_types.push_back(GetLlvmTypeForTypeId(ctx, root.type, types));
  }

  std::string name = std::format("ProcessFrame{}", process_index);
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

auto BuildSlotInfoFromDesign(const mir::Design& design, const TypeArena& types)
    -> std::vector<SlotInfo> {
  std::vector<SlotInfo> slots;
  slots.reserve(design.slot_table.size());

  for (size_t i = 0; i < design.slot_table.size(); ++i) {
    TypeId type_id = design.slot_table[i];
    const Type& type = types[type_id];

    SlotTypeInfo type_info{};
    if (type.Kind() == TypeKind::kReal) {
      type_info = {
          .kind = VarTypeKind::kReal,
          .width = 64,
          .is_signed = true,
          .is_four_state = false,
      };
    } else if (type.Kind() == TypeKind::kString) {
      type_info = {
          .kind = VarTypeKind::kString,
          .width = 0,
          .is_signed = false,
          .is_four_state = false,
      };
    } else if (IsPacked(type)) {
      uint32_t width = PackedBitWidth(type, types);
      bool is_signed = IsPackedSigned(type, types);
      bool is_four_state = IsPackedFourState(type, types);
      type_info = {
          .kind = VarTypeKind::kIntegral,
          .width = width > 0 ? width : 32,
          .is_signed = is_signed,
          .is_four_state = is_four_state,
      };
    } else {
      // Unsupported type - use placeholder for SlotTypeInfo
      // The actual TypeId is preserved for LLVM type derivation
      type_info = {
          .kind = VarTypeKind::kIntegral,
          .width = 32,
          .is_signed = false,
          .is_four_state = false,
      };
    }

    slots.push_back(
        SlotInfo{
            .slot_id = mir::SlotId{static_cast<uint32_t>(i)},
            .type_id = type_id,
            .type_info = type_info,
        });
  }

  return slots;
}

auto BuildLayout(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const std::vector<SlotInfo>& slots, llvm::LLVMContext& ctx) -> Layout {
  Layout layout;

  // Build runtime types
  layout.suspend_record_type = BuildSuspendRecordType(ctx);
  layout.header_type = BuildHeaderType(ctx, layout.suspend_record_type);

  // Build design layout (use actual TypeIds for type derivation)
  layout.design = BuildDesignLayout(slots, types, ctx);

  // Phase 1: Collect init processes (package variable initialization)
  // These run synchronously before scheduling, in design.init_processes order
  for (mir::ProcessId proc_id : design.init_processes) {
    layout.process_ids.push_back(proc_id);
  }
  layout.num_init_processes = layout.process_ids.size();

  // Phase 2: Collect connection processes (scheduled, always_comb semantics)
  // These implement port drive bindings (kDriveParentToChild,
  // kDriveChildToParent)
  for (mir::ProcessId proc_id : design.connection_processes) {
    layout.process_ids.push_back(proc_id);
  }

  // Phase 3: Collect module processes (run through scheduler)
  for (const auto& element : design.elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    for (mir::ProcessId proc_id : mir_module.processes) {
      const auto& process = arena[proc_id];
      if (process.kind != mir::ProcessKind::kFinal) {
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

    // Collect frame roots (de-duplicated by root identity)
    auto frame_roots = CollectFrameRoots(process, arena, types);

    // Build frame layout
    proc_layout.frame = BuildFrameLayout(frame_roots, types, ctx, i);

    // Build process state type
    proc_layout.state_type = BuildProcessStateType(
        ctx, layout.header_type, proc_layout.frame.llvm_type, i);

    layout.processes.push_back(std::move(proc_layout));
  }

  return layout;
}

}  // namespace lyra::lowering::mir_to_llvm
