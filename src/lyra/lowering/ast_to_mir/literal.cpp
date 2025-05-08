#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <slang/ast/expressions/LiteralExpressions.h>

#include "lyra/lowering/ast_to_mir/type.hpp"

namespace lyra::lowering {

auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> common::Literal {
  auto value = literal.getValue().as<int>().value();
  auto type = LowerType(*literal.type);
  return {type, common::ValueStorage(value)};
}

auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal {
  auto value = std::string(literal.getValue());
  auto type = common::Type::String();
  return {type, common::ValueStorage(std::move(value))};
}

}  // namespace lyra::lowering
