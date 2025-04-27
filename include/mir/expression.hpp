#pragma once

#include <memory>
#include <string>

namespace lyra::mir {

class Expression {
 public:
  enum class Kind {
    kLiteral,
    kIdentifier,
    kBinary,
  };

  Kind kind;

  Expression(const Expression &) = default;
  Expression(Expression &&) = delete;
  auto operator=(const Expression &) -> Expression & = default;
  auto operator=(Expression &&) -> Expression & = delete;
  explicit Expression(Kind kind) : kind(kind) {
  }
  virtual ~Expression() = default;
};

class LiteralExpression : public Expression {
 public:
  int value;

  explicit LiteralExpression(int v) : Expression(Kind::kLiteral), value(v) {
  }
};

class IdentifierExpression : public Expression {
 public:
  std::string name;

  explicit IdentifierExpression(std::string n)
      : Expression(Kind::kIdentifier), name(std::move(n)) {
  }
};

class BinaryExpression : public Expression {
 public:
  enum class Operator { kAdd };

  Operator op;
  std::shared_ptr<Expression> left;
  std::shared_ptr<Expression> right;

  BinaryExpression(
      Operator op, std::shared_ptr<Expression> left,
      std::shared_ptr<Expression> right)
      : Expression(Kind::kBinary),
        op(op),
        left(std::move(left)),
        right(std::move(right)) {
  }
};

}  // namespace lyra::mir
