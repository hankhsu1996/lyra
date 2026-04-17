#include "lyra/llvm_backend/init_descriptor_utils.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
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

namespace {

auto DescribeStorageSpecKind(const SlotStorageSpec& spec) -> std::string {
  return std::visit(
      common::Overloaded{
          [](const PackedStorageSpec&) -> std::string {
            return "PackedStorageSpec";
          },
          [](const FloatStorageSpec&) -> std::string {
            return "FloatStorageSpec";
          },
          [](const ArrayStorageSpec&) -> std::string {
            return "ArrayStorageSpec";
          },
          [](const StructStorageSpec&) -> std::string {
            return "StructStorageSpec";
          },
          [](const UnionStorageSpec&) -> std::string {
            return "UnionStorageSpec";
          },
          [](const HandleStorageSpec&) -> std::string {
            return "HandleStorageSpec";
          },
      },
      spec.data);
}

// Lower a RealConstant to canonical float storage bytes.
// Runtime float storage uses native IEEE 754 representation:
// 8 bytes for real (double), 4 bytes for shortreal (float).
void LowerRealConstantToCanonicalBytes(
    const RealConstant& rc, const FloatStorageSpec& fspec,
    std::vector<uint8_t>& out) {
  auto byte_size = fspec.layout.total_byte_size;
  auto start = out.size();
  out.resize(start + byte_size, 0);
  if (byte_size == 8) {
    double val = rc.value;
    std::memcpy(&out[start], &val, 8);
  } else if (byte_size == 4) {
    auto val = static_cast<float>(rc.value);
    std::memcpy(&out[start], &val, 4);
  } else {
    throw common::InternalError(
        "LowerRealConstantToCanonicalBytes",
        std::format("unsupported FloatStorageSpec byte_size {}", byte_size));
  }
}

// Forward declaration for recursive lowering.
void LowerConstantToCanonicalBytesImpl(
    const Constant& constant, const SlotStorageSpec& spec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out);

void LowerStructConstantToCanonicalBytes(
    const StructConstant& sc, const StructStorageSpec& sspec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out) {
  if (sc.fields.size() != sspec.fields.size()) {
    throw common::InternalError(
        "LowerStructConstantToCanonicalBytes",
        std::format(
            "field count mismatch: StructConstant has {} fields, "
            "StructStorageSpec has {}",
            sc.fields.size(), sspec.fields.size()));
  }
  auto start = out.size();
  // Allocate the full struct storage (includes alignment padding).
  out.resize(start + sspec.layout.total_byte_size, 0);

  for (size_t i = 0; i < sc.fields.size(); ++i) {
    const auto& field_constant = constant_arena[sc.fields[i]];
    const auto& field_spec = storage_arena.Get(sspec.fields[i].field_spec_id);
    uint32_t field_offset = sspec.fields[i].byte_offset;

    // Lower field into a temporary buffer, then copy to the right offset.
    std::vector<uint8_t> field_bytes;
    LowerConstantToCanonicalBytesImpl(
        field_constant, field_spec, constant_arena, storage_arena, field_bytes);

    if (field_bytes.size() != field_spec.TotalByteSize()) {
      throw common::InternalError(
          "LowerStructConstantToCanonicalBytes",
          std::format(
              "field {} produced {} bytes, expected {}", i, field_bytes.size(),
              field_spec.TotalByteSize()));
    }
    std::memcpy(
        &out[start + field_offset], field_bytes.data(), field_bytes.size());
  }
}

void LowerArrayConstantToCanonicalBytes(
    const ArrayConstant& ac, const ArrayStorageSpec& aspec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out) {
  if (ac.elements.size() != aspec.element_count) {
    throw common::InternalError(
        "LowerArrayConstantToCanonicalBytes",
        std::format(
            "element count mismatch: ArrayConstant has {} elements, "
            "ArrayStorageSpec has {}",
            ac.elements.size(), aspec.element_count));
  }
  auto start = out.size();
  out.resize(start + aspec.layout.total_byte_size, 0);

  const auto& elem_spec = storage_arena.Get(aspec.element_spec_id);

  for (uint32_t i = 0; i < aspec.element_count; ++i) {
    const auto& elem_constant = constant_arena[ac.elements[i]];
    uint32_t elem_offset = i * aspec.element_stride;

    std::vector<uint8_t> elem_bytes;
    LowerConstantToCanonicalBytesImpl(
        elem_constant, elem_spec, constant_arena, storage_arena, elem_bytes);

    if (elem_bytes.size() != elem_spec.TotalByteSize()) {
      throw common::InternalError(
          "LowerArrayConstantToCanonicalBytes",
          std::format(
              "element {} produced {} bytes, expected {}", i, elem_bytes.size(),
              elem_spec.TotalByteSize()));
    }
    std::memcpy(
        &out[start + elem_offset], elem_bytes.data(), elem_bytes.size());
  }
}

void LowerConstantToCanonicalBytesImpl(
    const Constant& constant, const SlotStorageSpec& spec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out) {
  std::visit(
      common::Overloaded{
          [&](const IntegralConstant& ic) {
            LowerIntegralConstantToCanonicalBytes(ic, spec, out);
          },
          [&](const RealConstant& rc) {
            const auto* fspec = std::get_if<FloatStorageSpec>(&spec.data);
            if (fspec == nullptr) {
              throw common::InternalError(
                  "LowerConstantToCanonicalBytes",
                  std::format(
                      "RealConstant requires FloatStorageSpec, got {}",
                      DescribeStorageSpecKind(spec)));
            }
            LowerRealConstantToCanonicalBytes(rc, *fspec, out);
          },
          [&](const StructConstant& sc) {
            const auto* sspec = std::get_if<StructStorageSpec>(&spec.data);
            if (sspec == nullptr) {
              throw common::InternalError(
                  "LowerConstantToCanonicalBytes",
                  std::format(
                      "StructConstant requires StructStorageSpec, got {}",
                      DescribeStorageSpecKind(spec)));
            }
            LowerStructConstantToCanonicalBytes(
                sc, *sspec, constant_arena, storage_arena, out);
          },
          [&](const ArrayConstant& ac) {
            const auto* aspec = std::get_if<ArrayStorageSpec>(&spec.data);
            if (aspec == nullptr) {
              throw common::InternalError(
                  "LowerConstantToCanonicalBytes",
                  std::format(
                      "ArrayConstant requires ArrayStorageSpec, got {}",
                      DescribeStorageSpecKind(spec)));
            }
            LowerArrayConstantToCanonicalBytes(
                ac, *aspec, constant_arena, storage_arena, out);
          },
          [&](const StringConstant&) {
            throw common::InternalError(
                "LowerConstantToCanonicalBytes",
                std::format(
                    "unsupported constant/storage: StringConstant -> {} "
                    "(string port constants require runtime string pool "
                    "allocation, not supported for construction-time init)",
                    DescribeStorageSpecKind(spec)));
          },
          [&](const NullConstant&) {
            throw common::InternalError(
                "LowerConstantToCanonicalBytes",
                std::format(
                    "unsupported constant/storage: NullConstant -> {} "
                    "(no storage representation for null constants)",
                    DescribeStorageSpecKind(spec)));
          },
      },
      constant.value);
}

}  // namespace

void LowerConstantToCanonicalBytes(
    const Constant& constant, const SlotStorageSpec& spec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out) {
  LowerConstantToCanonicalBytesImpl(
      constant, spec, constant_arena, storage_arena, out);
}

}  // namespace lyra::lowering::mir_to_llvm
