#include "lyra/lowering/ast_to_mir/type.hpp"

namespace lyra::lowering {

using Type = common::Type;

auto LowerType(const slang::ast::Type& type) -> Type {
  if (type.isString()) {
    return Type::String();
  }

  if (type.isVoid()) {
    return Type::Void();
  }

  if (type.isIntegral() && !type.isFourState()) {
    auto width = type.getBitWidth();
    bool is_signed = type.isSigned();

    if (width == 1) {
      return Type::TwoState(1, is_signed);
    }
    if (width == 32) {
      return Type::TwoState(32, is_signed);
    }
    if (width == 64) {
      return Type::TwoState(64, is_signed);
    }

    throw std::runtime_error{fmt::format(
        "Unsupported integral type width {} in AST to MIR LowerType", width)};
  }

  throw std::runtime_error{fmt::format(
      "Unsupported type {} in AST to MIR LowerType", type.toString())};
}

}  // namespace lyra::lowering
