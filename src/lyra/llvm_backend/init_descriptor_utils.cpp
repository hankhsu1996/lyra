#include "lyra/llvm_backend/init_descriptor_utils.hpp"

#include <cstdint>
#include <cstring>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"

namespace lyra::lowering::mir_to_llvm {

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
