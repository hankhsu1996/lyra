#include "lyra/llvm_backend/init_descriptor_utils.hpp"

#include <cstdint>
#include <cstring>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/runtime/body_realization_desc.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void FlattenImpl(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint64_t base_offset, std::vector<runtime::InitPatchEntry>& out) {
  std::visit(
      common::Overloaded{
          [&](const PackedStorageSpec& packed) {
            if (!packed.is_four_state) return;
            uint32_t lane_bytes = packed.LaneByteSize();
            uint32_t unk_offset = packed.UnknownLaneOffset();
            uint32_t mask_width = lane_bytes * 8;
            uint64_t mask = packed.bit_width >= mask_width
                                ? ~uint64_t{0}
                                : (uint64_t{1} << packed.bit_width) - 1;
            out.push_back(
                runtime::InitPatchEntry{
                    .rel_byte_offset = NarrowToU32(
                        base_offset + unk_offset, "FlattenFourStatePatches"),
                    .byte_width = lane_bytes,
                    .mask = mask,
                });
          },
          [](const FloatStorageSpec&) {},
          [&](const ArrayStorageSpec& arr) {
            const auto& elem_spec = arena.Get(arr.element_spec_id);
            if (!HasFourStateContent(elem_spec, arena)) return;
            for (uint32_t i = 0; i < arr.element_count; ++i) {
              uint64_t elem_offset =
                  base_offset + static_cast<uint64_t>(i) * arr.element_stride;
              FlattenImpl(elem_spec, arena, elem_offset, out);
            }
          },
          [&](const StructStorageSpec& s) {
            for (const auto& field : s.fields) {
              const auto& field_spec = arena.Get(field.field_spec_id);
              if (!HasFourStateContent(field_spec, arena)) continue;
              FlattenImpl(
                  field_spec, arena, base_offset + field.byte_offset, out);
            }
          },
          [&](const UnionStorageSpec& u) {
            if (!u.has_four_state_content) return;
            // Unions use zero-fill normalization. After memset(0), the
            // entire union storage is already zero. X-initialization of
            // unions is not supported (matches EmitSVDefaultInitImpl).
          },
          [](const HandleStorageSpec&) {},
      },
      spec.data);
}

}  // namespace

void FlattenFourStatePatches(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint64_t base_rel_offset, std::vector<runtime::InitPatchEntry>& out) {
  FlattenImpl(spec, arena, base_rel_offset, out);
}

void LowerIntegralConstantToCanonicalBytes(
    const IntegralConstant& value, const SlotStorageSpec& spec,
    std::vector<uint8_t>& out) {
  const auto* packed = std::get_if<PackedStorageSpec>(&spec.data);
  if (packed == nullptr) {
    throw common::InternalError(
        "LowerIntegralConstantToCanonicalBytes",
        "only PackedStorageSpec is supported for parameter lowering");
  }

  uint32_t total_bytes = spec.TotalByteSize();
  size_t out_start = out.size();
  out.resize(out_start + total_bytes, 0);

  uint32_t lane_bytes = packed->LaneByteSize();

  // Write value bits into known lane (offset 0).
  {
    uint32_t words_needed = (lane_bytes + 7) / 8;
    for (uint32_t w = 0; w < words_needed && w < value.value.size(); ++w) {
      uint64_t word = value.value[w];
      uint32_t byte_start = w * 8;
      uint32_t bytes_this_word = std::min(uint32_t{8}, lane_bytes - byte_start);
      std::memcpy(&out[out_start + byte_start], &word, bytes_this_word);
    }
  }

  // Write unknown bits into unknown lane (4-state only).
  if (packed->is_four_state) {
    uint32_t unk_offset = packed->UnknownLaneOffset();
    uint32_t words_needed = (lane_bytes + 7) / 8;
    for (uint32_t w = 0; w < words_needed && w < value.unknown.size(); ++w) {
      uint64_t word = value.unknown[w];
      uint32_t byte_start = w * 8;
      uint32_t bytes_this_word = std::min(uint32_t{8}, lane_bytes - byte_start);
      std::memcpy(
          &out[out_start + unk_offset + byte_start], &word, bytes_this_word);
    }
  }

  // Mask tail bits in the final byte of each lane when bit_width is
  // not byte-aligned. Prevents leaking garbage high bits into canonical
  // storage.
  uint32_t rem_bits = packed->bit_width % 8;
  if (rem_bits != 0) {
    auto tail_mask = static_cast<uint8_t>((uint32_t{1} << rem_bits) - 1);
    uint32_t last_byte = lane_bytes - 1;
    out[out_start + last_byte] &= tail_mask;
    if (packed->is_four_state) {
      uint32_t unk_offset = packed->UnknownLaneOffset();
      out[out_start + unk_offset + last_byte] &= tail_mask;
    }
  }
}

}  // namespace lyra::lowering::mir_to_llvm
