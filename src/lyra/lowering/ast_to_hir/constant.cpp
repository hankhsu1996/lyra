#include "lyra/lowering/ast_to_hir/constant.hpp"

#include <cstdint>
#include <span>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerSVIntToIntegralConstant(const slang::SVInt& sv_int)
    -> IntegralConstant {
  bool has_unknown = sv_int.hasUnknown();
  uint32_t total_words = sv_int.getNumWords();

  if (has_unknown && total_words % 2 != 0) {
    throw common::InternalError(
        "constant lowering",
        "slang storage assumption violated: 4-state integer must have even "
        "word count");
  }

  uint32_t value_word_count = has_unknown ? total_words / 2 : total_words;

  std::span<const uint64_t> raw_data(sv_int.getRawPtr(), total_words);
  std::span<const uint64_t> value_words = raw_data.subspan(0, value_word_count);

  IntegralConstant constant;
  constant.value.assign(value_words.begin(), value_words.end());

  if (has_unknown) {
    std::span<const uint64_t> unknown_words =
        raw_data.subspan(value_word_count);
    constant.unknown.assign(unknown_words.begin(), unknown_words.end());
  } else {
    constant.unknown.resize(value_word_count, 0);
  }

  return constant;
}

auto LowerIntegralConstant(
    const slang::SVInt& sv_int, TypeId type, Context* ctx) -> ConstId {
  IntegralConstant constant = LowerSVIntToIntegralConstant(sv_int);
  return ctx->constant_arena->Intern(type, std::move(constant));
}

namespace {

// Tick literal fill kind (what value to replicate).
enum class TickFillKind { kZero, kOne, kX, kZ };

// Extract the fill kind from an unbased-unsized tick literal.
// Uses slang's public API (exactlyEqual) to avoid internal encoding
// dependencies.
auto ExtractTickFillKind(const slang::SVInt& tick_value) -> TickFillKind {
  slang::logic_t bit = tick_value[0];
  if (exactlyEqual(bit, slang::logic_t::x)) {
    return TickFillKind::kX;
  }
  if (exactlyEqual(bit, slang::logic_t::z)) {
    return TickFillKind::kZ;
  }
  // Known bit: 0 or 1
  return bit.value == 0 ? TickFillKind::kZero : TickFillKind::kOne;
}

// Map tick fill kind to (value_bit, unknown_bit) for 4-state encoding.
// Lyra's canonical 4-state encoding:
//   value=0, unknown=0 -> 0
//   value=1, unknown=0 -> 1
//   value=0, unknown=1 -> X
//   value=1, unknown=1 -> Z
struct FillBits {
  bool value_bit;
  bool unknown_bit;
};

auto TickFillKindToBits(TickFillKind kind) -> FillBits {
  switch (kind) {
    case TickFillKind::kZero:
      return {.value_bit = false, .unknown_bit = false};
    case TickFillKind::kOne:
      return {.value_bit = true, .unknown_bit = false};
    case TickFillKind::kX:
      return {.value_bit = false, .unknown_bit = true};
    case TickFillKind::kZ:
      return {.value_bit = true, .unknown_bit = true};
  }
  throw common::InternalError("TickFillKindToBits", "invalid fill kind");
}

// Create a filled IntegralConstant with all bits set to the same value.
auto MakeFilledIntegralConstant(
    uint32_t width, bool value_bit, bool unknown_bit) -> IntegralConstant {
  uint32_t word_count = (width + 63) / 64;
  IntegralConstant constant;
  constant.value.resize(word_count);
  constant.unknown.resize(word_count);

  uint64_t value_fill = value_bit ? ~uint64_t{0} : uint64_t{0};
  uint64_t unknown_fill = unknown_bit ? ~uint64_t{0} : uint64_t{0};

  for (uint32_t i = 0; i < word_count; ++i) {
    constant.value[i] = value_fill;
    constant.unknown[i] = unknown_fill;
  }

  // Mask the last word to the actual bit width
  uint32_t excess_bits = (word_count * 64) - width;
  if (excess_bits > 0) {
    uint64_t mask = (~uint64_t{0}) >> excess_bits;
    constant.value[word_count - 1] &= mask;
    constant.unknown[word_count - 1] &= mask;
  }

  return constant;
}

}  // namespace

auto CreateFilledConstant(
    const slang::SVInt& tick_value, TypeId target_type, Context* ctx)
    -> ConstId {
  const Type& target = (*ctx->type_arena)[target_type];
  uint32_t width = PackedBitWidth(target, *ctx->type_arena);
  bool target_is_four_state = IsPackedFourState(target, *ctx->type_arena);

  // Determine fill kind from the tick literal's bit0.
  TickFillKind fill_kind = ExtractTickFillKind(tick_value);
  FillBits bits = TickFillKindToBits(fill_kind);

  // 2-state target: X/Z become 0 (SV X-pessimism semantics)
  if (!target_is_four_state && bits.unknown_bit) {
    bits.value_bit = false;
    bits.unknown_bit = false;
  }

  IntegralConstant constant =
      MakeFilledIntegralConstant(width, bits.value_bit, bits.unknown_bit);
  return ctx->constant_arena->Intern(target_type, std::move(constant));
}

}  // namespace lyra::lowering::ast_to_hir
