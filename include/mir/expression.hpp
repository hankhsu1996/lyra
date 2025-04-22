#pragma once

#include <memory>
#include <string>
#include <variant>

namespace volans::mir {

class Expression {
 public:
  enum class Kind { kLiteralInt, kLiteralString, kIdentifier, kAdd };

  Kind kind;

  // variant stores the payload depending on kind
  std::variant<
      int, std::string,
      std::pair<std::shared_ptr<Expression>, std::shared_ptr<Expression>>>
      value;
};

}  // namespace volans::mir
