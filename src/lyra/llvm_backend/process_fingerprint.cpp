#include "lyra/llvm_backend/process_fingerprint.hpp"

#include <cstdint>
#include <cstring>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// FNV-1a 64-bit hash
class Hasher {
 public:
  void Mix(uint64_t v) {
    state_ ^= v;
    state_ *= kFnvPrime;
  }

  void Mix32(uint32_t v) {
    Mix(static_cast<uint64_t>(v));
  }
  void Mix8(uint8_t v) {
    Mix(static_cast<uint64_t>(v));
  }
  void MixBool(bool v) {
    Mix8(v ? 1 : 0);
  }

  void MixInt(int v) {
    uint32_t bits = 0;
    std::memcpy(&bits, &v, sizeof(bits));
    Mix32(bits);
  }

  void MixInt64(int64_t v) {
    uint64_t bits = 0;
    std::memcpy(&bits, &v, sizeof(bits));
    Mix(bits);
  }

  void MixDouble(double v) {
    uint64_t bits = 0;
    std::memcpy(&bits, &v, sizeof(bits));
    Mix(bits);
  }

  void MixString(const std::string& s) {
    Mix(s.size());
    for (char c : s) {
      Mix8(static_cast<uint8_t>(c));
    }
  }

  [[nodiscard]] auto Finish() const -> uint64_t {
    return state_;
  }

 private:
  static constexpr uint64_t kFnvOffset = 14695981039346656037ULL;
  static constexpr uint64_t kFnvPrime = 1099511628211ULL;
  uint64_t state_ = kFnvOffset;
};

// Hash the structural shape of a type (not the TypeId value, which is an
// interned pointer that may differ across instances).
void HashTypeShape(Hasher& h, TypeId type_id, const TypeArena& types) {
  const Type& type = types[type_id];
  h.Mix8(static_cast<uint8_t>(type.Kind()));

  switch (type.Kind()) {
    case TypeKind::kVoid:
      break;
    case TypeKind::kIntegral: {
      const auto& info = type.AsIntegral();
      h.Mix32(info.bit_width);
      h.MixBool(info.is_signed);
      h.MixBool(info.is_four_state);
      break;
    }
    case TypeKind::kReal:
    case TypeKind::kShortReal:
    case TypeKind::kString:
      break;
    case TypeKind::kPackedArray: {
      const auto& info = type.AsPackedArray();
      h.Mix32(info.range.Size());
      HashTypeShape(h, info.element_type, types);
      break;
    }
    case TypeKind::kPackedStruct: {
      const auto& info = type.AsPackedStruct();
      h.Mix(info.fields.size());
      for (const auto& field : info.fields) {
        HashTypeShape(h, field.type, types);
      }
      h.Mix32(info.total_bit_width);
      h.MixBool(info.is_signed);
      h.MixBool(info.is_four_state);
      break;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      h.Mix32(info.range.Size());
      HashTypeShape(h, info.element_type, types);
      break;
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      h.Mix(info.fields.size());
      for (const auto& field : info.fields) {
        HashTypeShape(h, field.type, types);
      }
      break;
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      h.Mix(info.members.size());
      for (const auto& member : info.members) {
        HashTypeShape(h, member.type, types);
      }
      break;
    }
    case TypeKind::kDynamicArray: {
      HashTypeShape(h, type.AsDynamicArray().element_type, types);
      break;
    }
    case TypeKind::kQueue: {
      HashTypeShape(h, type.AsQueue().element_type, types);
      break;
    }
    case TypeKind::kAssociativeArray: {
      const auto& info = type.AsAssociativeArray();
      HashTypeShape(h, info.element_type, types);
      HashTypeShape(h, info.key_type, types);
      break;
    }
    case TypeKind::kEnum: {
      const auto& info = type.AsEnum();
      HashTypeShape(h, info.base_type, types);
      h.Mix(info.members.size());
      break;
    }
  }
}

void HashPlaceId(
    Hasher& h, mir::PlaceId place_id, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id);

void HashOperand(
    Hasher& h, const mir::Operand& op, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  h.Mix8(static_cast<uint8_t>(op.kind));

  switch (op.kind) {
    case mir::Operand::Kind::kConst: {
      const auto& c = std::get<Constant>(op.payload);
      HashTypeShape(h, c.type, types);
      std::visit(
          [&h](const auto& val) {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, IntegralConstant>) {
              h.Mix(val.value.size());
              for (uint64_t w : val.value) h.Mix(w);
              for (uint64_t w : val.unknown) h.Mix(w);
            } else if constexpr (std::is_same_v<T, StringConstant>) {
              h.MixString(val.value);
            } else if constexpr (std::is_same_v<T, RealConstant>) {
              h.MixDouble(val.value);
            } else if constexpr (std::is_same_v<T, StructConstant>) {
              h.Mix(val.fields.size());
              for (auto fid : val.fields) h.Mix32(fid.value);
            } else if constexpr (std::is_same_v<T, ArrayConstant>) {
              h.Mix(val.elements.size());
              for (auto eid : val.elements) h.Mix32(eid.value);
            }
          },
          c.value);
      break;
    }
    case mir::Operand::Kind::kUse: {
      auto place_id = std::get<mir::PlaceId>(op.payload);
      HashPlaceId(h, place_id, arena, types, base_slot_id);
      break;
    }
    case mir::Operand::Kind::kUseTemp: {
      auto temp_id = std::get<mir::TempId>(op.payload);
      h.MixInt(temp_id.value);
      break;
    }
    case mir::Operand::Kind::kPoison:
      break;
  }
}

void HashPlace(
    Hasher& h, const mir::Place& place, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  // Hash root (normalized for design slots)
  h.Mix8(static_cast<uint8_t>(place.root.kind));
  if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
    // Normalize: relative to base_slot_id
    h.MixInt(place.root.id - static_cast<int>(base_slot_id));
  } else {
    h.MixInt(place.root.id);
  }
  HashTypeShape(h, place.root.type, types);

  // Hash projections
  h.Mix(place.projections.size());
  for (const auto& proj : place.projections) {
    std::visit(
        [&h, &arena, &types, base_slot_id](const auto& info) {
          using T = std::decay_t<decltype(info)>;
          if constexpr (std::is_same_v<T, mir::FieldProjection>) {
            h.Mix8(0);
            h.MixInt(info.field_index);
          } else if constexpr (std::is_same_v<T, mir::IndexProjection>) {
            h.Mix8(1);
            HashOperand(h, info.index, arena, types, base_slot_id);
          } else if constexpr (std::is_same_v<T, mir::SliceProjection>) {
            h.Mix8(2);
            HashOperand(h, info.start, arena, types, base_slot_id);
            h.MixInt(info.width);
          } else if constexpr (std::is_same_v<T, mir::DerefProjection>) {
            h.Mix8(3);
          } else if constexpr (std::is_same_v<T, mir::BitRangeProjection>) {
            h.Mix8(4);
            HashOperand(h, info.bit_offset, arena, types, base_slot_id);
            h.Mix32(info.width);
            HashTypeShape(h, info.element_type, types);
          } else if constexpr (std::is_same_v<T, mir::UnionMemberProjection>) {
            h.Mix8(5);
            h.Mix32(info.member_index);
          }
        },
        proj.info);
  }
}

void HashPlaceId(
    Hasher& h, mir::PlaceId place_id, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  const auto& place = arena[place_id];
  HashPlace(h, place, arena, types, base_slot_id);
}

void HashTypedOperand(
    Hasher& h, const mir::TypedOperand& top, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  HashOperand(h, top.operand, arena, types, base_slot_id);
  HashTypeShape(h, top.type, types);
}

void HashFormatOp(
    Hasher& h, const mir::FormatOp& op, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  h.Mix8(static_cast<uint8_t>(op.kind));
  if (op.value) {
    h.MixBool(true);
    HashOperand(h, *op.value, arena, types, base_slot_id);
  } else {
    h.MixBool(false);
  }
  h.MixString(op.literal);
  HashTypeShape(h, op.type, types);
  h.MixBool(op.mods.left_align);
  h.MixBool(op.mods.zero_pad);
  h.MixBool(op.mods.width.has_value());
  if (op.mods.width) h.MixInt(*op.mods.width);
  h.MixBool(op.mods.precision.has_value());
  if (op.mods.precision) h.MixInt(*op.mods.precision);
}

void HashRvalue(
    Hasher& h, const mir::Rvalue& rv, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  // Hash operands in order
  h.Mix(rv.operands.size());
  for (const auto& op : rv.operands) {
    HashOperand(h, op, arena, types, base_slot_id);
  }

  // Hash rvalue kind via visitor
  std::visit(
      [&h, &arena, &types, base_slot_id](const auto& info) {
        using T = std::decay_t<decltype(info)>;
        if constexpr (std::is_same_v<T, mir::UnaryRvalueInfo>) {
          h.Mix8(0);
          h.Mix8(static_cast<uint8_t>(info.op));
        } else if constexpr (std::is_same_v<T, mir::BinaryRvalueInfo>) {
          h.Mix8(1);
          h.Mix8(static_cast<uint8_t>(info.op));
        } else if constexpr (std::is_same_v<T, mir::CastRvalueInfo>) {
          h.Mix8(2);
          HashTypeShape(h, info.source_type, types);
          HashTypeShape(h, info.target_type, types);
        } else if constexpr (std::is_same_v<T, mir::BitCastRvalueInfo>) {
          h.Mix8(3);
          HashTypeShape(h, info.source_type, types);
          HashTypeShape(h, info.target_type, types);
        } else if constexpr (std::is_same_v<T, mir::AggregateRvalueInfo>) {
          h.Mix8(4);
          HashTypeShape(h, info.result_type, types);
        } else if constexpr (std::is_same_v<T, mir::BuiltinCallRvalueInfo>) {
          h.Mix8(5);
          h.Mix8(static_cast<uint8_t>(info.method));
          HashTypeShape(h, info.result_type, types);
          h.MixBool(info.receiver.has_value());
          if (info.receiver) {
            HashPlaceId(h, *info.receiver, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::IndexValidityRvalueInfo>) {
          h.Mix8(6);
          h.MixInt64(info.lower_bound);
          h.MixInt64(info.upper_bound);
          h.MixBool(info.check_known);
        } else if constexpr (std::is_same_v<T, mir::GuardedUseRvalueInfo>) {
          h.Mix8(7);
          HashPlaceId(h, info.place, arena, types, base_slot_id);
          HashTypeShape(h, info.result_type, types);
        } else if constexpr (std::is_same_v<T, mir::ConcatRvalueInfo>) {
          h.Mix8(8);
          HashTypeShape(h, info.result_type, types);
        } else if constexpr (std::is_same_v<T, mir::ReplicateRvalueInfo>) {
          h.Mix8(9);
          HashTypeShape(h, info.result_type, types);
          h.Mix32(info.count);
        } else if constexpr (std::is_same_v<T, mir::SFormatRvalueInfo>) {
          h.Mix8(10);
          h.Mix(info.ops.size());
          for (const auto& op : info.ops) {
            HashFormatOp(h, op, arena, types, base_slot_id);
          }
          h.Mix8(static_cast<uint8_t>(info.default_format));
          h.MixBool(info.has_runtime_format);
        } else if constexpr (std::is_same_v<T, mir::TestPlusargsRvalueInfo>) {
          h.Mix8(11);
          HashTypedOperand(h, info.query, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::FopenRvalueInfo>) {
          h.Mix8(12);
          HashTypedOperand(h, info.filename, arena, types, base_slot_id);
          h.MixBool(info.mode.has_value());
          if (info.mode)
            HashTypedOperand(h, *info.mode, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::RuntimeQueryRvalueInfo>) {
          h.Mix8(13);
          h.Mix8(static_cast<uint8_t>(info.kind));
        } else if constexpr (std::is_same_v<T, mir::MathCallRvalueInfo>) {
          h.Mix8(14);
          h.Mix8(static_cast<uint8_t>(info.fn));
        } else if constexpr (std::is_same_v<T, mir::SystemTfRvalueInfo>) {
          h.Mix8(15);
          h.Mix32(static_cast<uint32_t>(info.opcode));
        } else if constexpr (std::is_same_v<T, mir::ArrayQueryRvalueInfo>) {
          h.Mix8(16);
          h.Mix8(static_cast<uint8_t>(info.kind));
          h.Mix8(info.total_dims);
          h.Mix8(info.unpacked_dims);
        } else if constexpr (std::is_same_v<T, mir::SystemCmdRvalueInfo>) {
          h.Mix8(17);
          h.MixBool(info.command.has_value());
          if (info.command) {
            HashTypedOperand(h, *info.command, arena, types, base_slot_id);
          }
        }
      },
      rv.info);
}

void HashRhs(
    Hasher& h, const mir::RightHandSide& rhs, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  std::visit(
      [&h, &arena, &types, base_slot_id](const auto& val) {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, mir::Operand>) {
          h.Mix8(0);
          HashOperand(h, val, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::Rvalue>) {
          h.Mix8(1);
          HashRvalue(h, val, arena, types, base_slot_id);
        }
      },
      rhs);
}

void HashEffect(
    Hasher& h, const mir::EffectOp& effect, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  std::visit(
      [&h, &arena, &types, base_slot_id](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        if constexpr (std::is_same_v<T, mir::DisplayEffect>) {
          h.Mix8(0);
          h.Mix8(static_cast<uint8_t>(op.print_kind));
          h.Mix(op.ops.size());
          for (const auto& fmt : op.ops) {
            HashFormatOp(h, fmt, arena, types, base_slot_id);
          }
          h.MixBool(op.descriptor.has_value());
          if (op.descriptor) {
            HashOperand(h, *op.descriptor, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::SeverityEffect>) {
          h.Mix8(1);
          h.Mix8(static_cast<uint8_t>(op.level));
          h.Mix(op.ops.size());
          for (const auto& fmt : op.ops) {
            HashFormatOp(h, fmt, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::MemIOEffect>) {
          h.Mix8(2);
          h.MixBool(op.is_read);
          h.MixBool(op.is_hex);
          HashPlaceId(h, op.target, arena, types, base_slot_id);
          HashTypeShape(h, op.target_type, types);
          HashTypedOperand(h, op.filename, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::TimeFormatEffect>) {
          h.Mix8(3);
          h.Mix8(static_cast<uint8_t>(op.units));
          h.MixInt(op.precision);
          h.MixString(op.suffix);
          h.MixInt(op.min_width);
        } else if constexpr (std::is_same_v<T, mir::SystemTfEffect>) {
          h.Mix8(4);
          h.Mix32(static_cast<uint32_t>(op.opcode));
          h.Mix(op.args.size());
          for (const auto& arg : op.args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::StrobeEffect>) {
          h.Mix8(5);
          h.Mix32(op.thunk.value);
        } else if constexpr (std::is_same_v<T, mir::MonitorEffect>) {
          h.Mix8(6);
          h.Mix32(op.setup_thunk.value);
          h.Mix32(op.check_thunk.value);
          h.Mix8(static_cast<uint8_t>(op.print_kind));
          h.Mix(op.format_ops.size());
          for (const auto& fmt : op.format_ops) {
            HashFormatOp(h, fmt, arena, types, base_slot_id);
          }
          h.Mix32(op.prev_buffer_size);
        } else if constexpr (std::is_same_v<T, mir::MonitorControlEffect>) {
          h.Mix8(7);
          h.MixBool(op.enable);
        } else if constexpr (std::is_same_v<T, mir::FillPackedEffect>) {
          h.Mix8(8);
          HashPlaceId(h, op.target, arena, types, base_slot_id);
          HashOperand(h, op.fill_value, arena, types, base_slot_id);
          h.Mix32(op.unit_bits);
          h.Mix32(op.count);
          h.Mix32(op.total_bits);
        }
      },
      effect);
}

void HashStatement(
    Hasher& h, const mir::Statement& stmt, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id,
    const std::vector<mir::FunctionId>& module_functions) {
  std::visit(
      [&h, &arena, &types, base_slot_id, &module_functions](const auto& data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, mir::Assign>) {
          h.Mix8(0);
          HashPlaceId(h, data.dest, arena, types, base_slot_id);
          HashRhs(h, data.rhs, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::GuardedAssign>) {
          h.Mix8(1);
          HashPlaceId(h, data.dest, arena, types, base_slot_id);
          HashRhs(h, data.rhs, arena, types, base_slot_id);
          HashOperand(h, data.guard, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::Effect>) {
          h.Mix8(2);
          HashEffect(h, data.op, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::DeferredAssign>) {
          h.Mix8(3);
          HashPlaceId(h, data.dest, arena, types, base_slot_id);
          HashRhs(h, data.rhs, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::Call>) {
          h.Mix8(4);
          // Hash callee
          std::visit(
              [&h, &module_functions](const auto& callee) {
                using C = std::decay_t<decltype(callee)>;
                if constexpr (std::is_same_v<C, mir::FunctionId>) {
                  h.Mix8(0);
                  // Normalize FunctionId to relative index in module
                  uint32_t rel_idx = UINT32_MAX;
                  for (uint32_t i = 0; i < module_functions.size(); ++i) {
                    if (module_functions[i] == callee) {
                      rel_idx = i;
                      break;
                    }
                  }
                  h.Mix32(rel_idx);
                } else {
                  h.Mix8(1);
                  h.Mix32(static_cast<uint32_t>(callee));
                }
              },
              data.callee);
          h.Mix(data.in_args.size());
          for (const auto& arg : data.in_args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
          h.MixBool(data.ret.has_value());
          if (data.ret) {
            HashPlaceId(h, data.ret->tmp, arena, types, base_slot_id);
            h.MixBool(data.ret->dest.has_value());
            if (data.ret->dest) {
              HashPlaceId(h, *data.ret->dest, arena, types, base_slot_id);
            }
            HashTypeShape(h, data.ret->type, types);
          }
          h.Mix(data.writebacks.size());
          for (const auto& wb : data.writebacks) {
            h.MixBool(wb.tmp.has_value());
            if (wb.tmp) {
              HashPlaceId(h, *wb.tmp, arena, types, base_slot_id);
            }
            HashPlaceId(h, wb.dest, arena, types, base_slot_id);
            HashTypeShape(h, wb.type, types);
            h.Mix8(static_cast<uint8_t>(wb.mode));
            h.Mix8(static_cast<uint8_t>(wb.kind));
            h.MixInt(wb.arg_index);
          }
        } else if constexpr (std::is_same_v<T, mir::BuiltinCall>) {
          h.Mix8(5);
          h.MixBool(data.dest.has_value());
          if (data.dest) {
            HashPlaceId(h, *data.dest, arena, types, base_slot_id);
          }
          h.Mix8(static_cast<uint8_t>(data.method));
          HashPlaceId(h, data.receiver, arena, types, base_slot_id);
          h.Mix(data.args.size());
          for (const auto& arg : data.args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::DefineTemp>) {
          h.Mix8(6);
          h.MixInt(data.temp_id);
          HashTypeShape(h, data.type, types);
          HashRhs(h, data.rhs, arena, types, base_slot_id);
        } else if constexpr (std::is_same_v<T, mir::AssocOp>) {
          h.Mix8(7);
          HashPlaceId(h, data.receiver, arena, types, base_slot_id);
          std::visit(
              [&h, &arena, &types, base_slot_id](const auto& op) {
                using U = std::decay_t<decltype(op)>;
                if constexpr (std::is_same_v<U, mir::AssocGet>) {
                  h.Mix8(0);
                  HashPlaceId(h, op.dest, arena, types, base_slot_id);
                  HashOperand(h, op.key, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocSet>) {
                  h.Mix8(1);
                  HashOperand(h, op.key, arena, types, base_slot_id);
                  HashOperand(h, op.value, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocExists>) {
                  h.Mix8(2);
                  HashPlaceId(h, op.dest, arena, types, base_slot_id);
                  HashOperand(h, op.key, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocDelete>) {
                  h.Mix8(3);
                } else if constexpr (std::is_same_v<U, mir::AssocDeleteKey>) {
                  h.Mix8(4);
                  HashOperand(h, op.key, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocNum>) {
                  h.Mix8(5);
                  HashPlaceId(h, op.dest, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocIterFirst>) {
                  h.Mix8(6);
                  HashPlaceId(h, op.dest_found, arena, types, base_slot_id);
                  HashPlaceId(h, op.out_key, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocIterLast>) {
                  h.Mix8(7);
                  HashPlaceId(h, op.dest_found, arena, types, base_slot_id);
                  HashPlaceId(h, op.out_key, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocIterNext>) {
                  h.Mix8(8);
                  HashPlaceId(h, op.dest_found, arena, types, base_slot_id);
                  HashPlaceId(h, op.key_place, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocIterPrev>) {
                  h.Mix8(9);
                  HashPlaceId(h, op.dest_found, arena, types, base_slot_id);
                  HashPlaceId(h, op.key_place, arena, types, base_slot_id);
                } else if constexpr (std::is_same_v<U, mir::AssocSnapshot>) {
                  h.Mix8(10);
                  HashPlaceId(h, op.dest_keys, arena, types, base_slot_id);
                }
              },
              data.data);
        }
      },
      stmt.data);
}

void HashWaitTrigger(
    Hasher& h, const mir::WaitTrigger& trigger, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  // Normalize signal_id
  h.Mix32(trigger.signal.value - base_slot_id);
  h.Mix8(static_cast<uint8_t>(trigger.edge));
  h.MixBool(trigger.observed_place.has_value());
  if (trigger.observed_place) {
    HashPlaceId(h, *trigger.observed_place, arena, types, base_slot_id);
  }
  h.MixBool(trigger.late_bound.has_value());
  if (trigger.late_bound) {
    const auto& lb = *trigger.late_bound;
    h.Mix(lb.dep_slots.size());
    for (auto slot : lb.dep_slots) {
      h.Mix32(slot.value - base_slot_id);
    }
    h.MixBool(lb.is_container);
    h.Mix32(lb.num_elements);
    h.MixBool(lb.element_type.has_value());
    if (lb.element_type) {
      HashTypeShape(h, *lb.element_type, types);
    }
  }
}

void HashTerminator(
    Hasher& h, const mir::Terminator& term, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id) {
  std::visit(
      [&h, &arena, &types, base_slot_id](const auto& data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, mir::Jump>) {
          h.Mix8(0);
          h.Mix32(data.target.value);
          h.Mix(data.args.size());
          for (const auto& arg : data.args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::Branch>) {
          h.Mix8(1);
          HashOperand(h, data.condition, arena, types, base_slot_id);
          h.Mix32(data.then_target.value);
          h.Mix(data.then_args.size());
          for (const auto& arg : data.then_args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
          h.Mix32(data.else_target.value);
          h.Mix(data.else_args.size());
          for (const auto& arg : data.else_args) {
            HashOperand(h, arg, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::Switch>) {
          h.Mix8(2);
          HashOperand(h, data.selector, arena, types, base_slot_id);
          h.Mix(data.targets.size());
          for (auto t : data.targets) h.Mix32(t.value);
        } else if constexpr (std::is_same_v<T, mir::QualifiedDispatch>) {
          h.Mix8(3);
          h.Mix8(static_cast<uint8_t>(data.qualifier));
          h.Mix8(static_cast<uint8_t>(data.statement_kind));
          h.Mix(data.conditions.size());
          for (const auto& c : data.conditions) {
            HashOperand(h, c, arena, types, base_slot_id);
          }
          h.Mix(data.targets.size());
          for (auto t : data.targets) h.Mix32(t.value);
          h.MixBool(data.has_else);
        } else if constexpr (std::is_same_v<T, mir::Delay>) {
          h.Mix8(4);
          h.Mix(data.ticks);
          h.Mix32(data.resume.value);
        } else if constexpr (std::is_same_v<T, mir::Wait>) {
          h.Mix8(5);
          h.Mix(data.triggers.size());
          for (const auto& trigger : data.triggers) {
            HashWaitTrigger(h, trigger, arena, types, base_slot_id);
          }
          h.Mix32(data.resume.value);
        } else if constexpr (std::is_same_v<T, mir::Return>) {
          h.Mix8(6);
          h.MixBool(data.value.has_value());
          if (data.value) {
            HashOperand(h, *data.value, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::Finish>) {
          h.Mix8(7);
          h.Mix8(static_cast<uint8_t>(data.kind));
          h.MixInt(data.level);
          h.MixBool(data.message.has_value());
          if (data.message) {
            HashOperand(h, *data.message, arena, types, base_slot_id);
          }
        } else if constexpr (std::is_same_v<T, mir::Repeat>) {
          h.Mix8(8);
        }
      },
      term.data);
}

}  // namespace

auto ComputeProcessFingerprint(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id,
    const std::vector<mir::FunctionId>& module_functions) -> uint64_t {
  Hasher h;

  // Hash process-level properties
  h.Mix8(static_cast<uint8_t>(process.kind));
  h.Mix(process.blocks.size());

  // Hash blocks in index order
  for (const auto& block : process.blocks) {
    // Hash block params
    h.Mix(block.params.size());
    for (const auto& param : block.params) {
      h.MixInt(param.temp_id);
      HashTypeShape(h, param.type, types);
    }

    // Hash statements in order
    h.Mix(block.statements.size());
    for (const auto& stmt : block.statements) {
      HashStatement(h, stmt, arena, types, base_slot_id, module_functions);
    }

    // Hash terminator
    HashTerminator(h, block.terminator, arena, types, base_slot_id);
  }

  return h.Finish();
}

}  // namespace lyra::lowering::mir_to_llvm
