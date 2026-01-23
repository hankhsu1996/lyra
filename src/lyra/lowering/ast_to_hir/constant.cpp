#include "lyra/lowering/ast_to_hir/constant.hpp"

#include <span>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerSVIntToIntegralConstant(const slang::SVInt& sv_int)
    -> IntegralConstant {
  bool has_unknown = sv_int.hasUnknown();
  uint32_t total_words = sv_int.getNumWords();

  // Slang layout: [value words | unknown words] when hasUnknown() is true.
  // Unknown words must be exactly half of total (value and unknown same size).
  if (has_unknown && total_words % 2 != 0) {
    throw common::InternalError(
        "constant lowering",
        "slang storage assumption violated: 4-state integer must have even "
        "word count");
  }

  uint32_t value_word_count = has_unknown ? total_words / 2 : total_words;

  std::span<const uint64_t> raw_data(sv_int.getRawPtr(), total_words);
  std::span<const uint64_t> value_words = raw_data.subspan(0, value_word_count);

  // Slang layout: [a_words | b_words] where a = value bits, b = unknown bits
  // This directly maps to our (value, unknown) encoding
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

}  // namespace lyra::lowering::ast_to_hir
