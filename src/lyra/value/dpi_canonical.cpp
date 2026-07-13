#include "lyra/value/dpi_canonical.hpp"

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

namespace lyra::value {

namespace {

// The 64-bit plane word a 32-bit canonical chunk lives in, and the shift within
// it: chunk 2k is the low half of word k, chunk 2k+1 the high half.
struct ChunkSlot {
  std::size_t word;
  unsigned shift;
};

[[nodiscard]] auto SlotForChunk(std::size_t chunk) -> ChunkSlot {
  return ChunkSlot{
      .word = chunk / 2, .shift = static_cast<unsigned>((chunk % 2) * 32U)};
}

[[nodiscard]] auto ChunkCount(std::uint64_t width) -> std::size_t {
  return static_cast<std::size_t>((width + 31U) / 32U);
}

[[nodiscard]] auto WordCount(std::uint64_t width) -> std::size_t {
  return static_cast<std::size_t>((width + 63U) / 64U);
}

[[nodiscard]] auto LowHalf(std::uint64_t word, unsigned shift)
    -> std::uint32_t {
  return static_cast<std::uint32_t>((word >> shift) & 0xFFFFFFFFU);
}

// Clears the bits above `width` in the top word of a plane so a canonical
// buffer's undetermined unused bits do not leak into the value.
auto MaskTopWord(std::vector<std::uint64_t>& words, std::uint64_t width)
    -> void {
  const auto top_bits = static_cast<unsigned>(width % 64U);
  if (top_bits != 0U && !words.empty()) {
    words.back() &= (std::uint64_t{1} << top_bits) - 1U;
  }
}

}  // namespace

auto ToSvLogic(const PackedArray& sv) -> unsigned char {
  const auto value = sv.ValueWords();
  const auto unknown = sv.UnknownWords();
  const unsigned v = value.empty() ? 0U : static_cast<unsigned>(value[0] & 1U);
  const unsigned u =
      unknown.empty() ? 0U : static_cast<unsigned>(unknown[0] & 1U);
  return static_cast<unsigned char>(v | (u << 1U));
}

auto FromSvLogic(unsigned char encoded, const PackedArray& prototype)
    -> PackedArray {
  const std::vector<std::uint64_t> value = {
      static_cast<std::uint64_t>(encoded & 1U)};
  const std::vector<std::uint64_t> unknown = {
      static_cast<std::uint64_t>((encoded >> 1U) & 1U)};
  return PackedArray::FromWords(
      value, unknown, prototype.BitWidth(), prototype.IsSigned(), true);
}

DpiBitBuffer::DpiBitBuffer(const PackedArray& sv)
    : chunks_(ChunkCount(sv.BitWidth())) {
  const auto value = sv.ValueWords();
  for (std::size_t i = 0; i < chunks_.size(); ++i) {
    const ChunkSlot slot = SlotForChunk(i);
    chunks_[i] = LowHalf(value[slot.word], slot.shift);
  }
}

auto DpiBitBuffer::Data() -> svBitVecVal* {
  return chunks_.data();
}

DpiLogicBuffer::DpiLogicBuffer(const PackedArray& sv)
    : chunks_(ChunkCount(sv.BitWidth())) {
  const auto value = sv.ValueWords();
  const auto unknown = sv.UnknownWords();
  for (std::size_t i = 0; i < chunks_.size(); ++i) {
    const ChunkSlot slot = SlotForChunk(i);
    chunks_[i].aval = LowHalf(value[slot.word], slot.shift);
    chunks_[i].bval =
        unknown.empty() ? 0U : LowHalf(unknown[slot.word], slot.shift);
  }
}

auto DpiLogicBuffer::Data() -> svLogicVecVal* {
  return chunks_.data();
}

auto ReadCanonicalBitVec(const svBitVecVal* src, const PackedArray& prototype)
    -> PackedArray {
  const std::uint64_t width = prototype.BitWidth();
  std::vector<std::uint64_t> value(WordCount(width), 0U);
  const std::size_t chunks = ChunkCount(width);
  const std::span<const svBitVecVal> in{src, chunks};
  for (std::size_t i = 0; i < chunks; ++i) {
    const ChunkSlot slot = SlotForChunk(i);
    value[slot.word] |= static_cast<std::uint64_t>(in[i]) << slot.shift;
  }
  MaskTopWord(value, width);
  return PackedArray::FromWords(value, {}, width, prototype.IsSigned(), false);
}

auto ReadCanonicalLogicVec(
    const svLogicVecVal* src, const PackedArray& prototype) -> PackedArray {
  const std::uint64_t width = prototype.BitWidth();
  std::vector<std::uint64_t> value(WordCount(width), 0U);
  std::vector<std::uint64_t> unknown(WordCount(width), 0U);
  const std::size_t chunks = ChunkCount(width);
  const std::span<const svLogicVecVal> in{src, chunks};
  for (std::size_t i = 0; i < chunks; ++i) {
    const ChunkSlot slot = SlotForChunk(i);
    value[slot.word] |= static_cast<std::uint64_t>(in[i].aval) << slot.shift;
    unknown[slot.word] |= static_cast<std::uint64_t>(in[i].bval) << slot.shift;
  }
  MaskTopWord(value, width);
  MaskTopWord(unknown, width);
  return PackedArray::FromWords(
      value, unknown, width, prototype.IsSigned(), true);
}

}  // namespace lyra::value
