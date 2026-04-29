#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

constexpr auto WordCount(std::uint32_t width) -> std::size_t {
  return (static_cast<std::size_t>(width) + 63U) / 64U;
}

auto MaskTopWord(std::vector<std::uint64_t>& words, std::uint32_t width)
    -> void {
  if (words.empty()) {
    return;
  }
  const std::uint32_t tail = width % 64U;
  if (tail == 0U) {
    return;
  }
  const std::uint64_t mask = (std::uint64_t{1} << tail) - 1U;
  words.back() &= mask;
}

}  // namespace

auto LowerSVIntToIntegralConstant(const slang::SVInt& sv)
    -> hir::IntegralConstant {
  const std::uint32_t width = sv.getBitWidth();
  if (width == 0U) {
    throw InternalError("LowerSVIntToIntegralConstant: zero-width SVInt");
  }
  const std::size_t words = WordCount(width);
  const bool four_state = sv.hasUnknown();

  hir::IntegralConstant out{
      .value_words = std::vector<std::uint64_t>(words, 0U),
      .state_words = four_state ? std::vector<std::uint64_t>(words, 0U)
                                : std::vector<std::uint64_t>{},
      .width = width,
      .signedness =
          sv.isSigned() ? hir::Signedness::kSigned : hir::Signedness::kUnsigned,
      .state_kind = four_state ? hir::IntegralStateKind::kFourState
                               : hir::IntegralStateKind::kTwoState,
  };

  const std::size_t total_words = four_state ? 2U * words : words;
  const std::span<const std::uint64_t> raw{sv.getRawPtr(), total_words};
  for (std::size_t i = 0; i < words; ++i) {
    out.value_words[i] = raw[i];
  }
  if (four_state) {
    // Slang's (v=0,u=1)=X, (v=1,u=1)=Z encoding becomes Lyra's
    // (v=0,s=1)=Z, (v=1,s=1)=X by XOR-ing value with the unknown plane.
    for (std::size_t i = 0; i < words; ++i) {
      const std::uint64_t slang_unknown = raw[words + i];
      out.state_words[i] = slang_unknown;
      out.value_words[i] ^= slang_unknown;
    }
  }

  MaskTopWord(out.value_words, width);
  if (four_state) {
    MaskTopWord(out.state_words, width);
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
