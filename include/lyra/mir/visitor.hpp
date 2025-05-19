#pragma once

namespace lyra::mir {

class Expression;
class LiteralExpression;
class IdentifierExpression;
class UnaryExpression;
class BinaryExpression;
class TernaryExpression;
class AssignmentExpression;
class ConversionExpression;
class SystemCallExpression;

class Statement;
class VariableDeclarationStatement;
class AssignStatement;
class ExpressionStatement;
class WaitEventStatement;
class DelayStatement;
class ConditionalStatement;
class WhileStatement;
class DoWhileStatement;
class BreakStatement;
class ContinueStatement;
class BlockStatement;

class MirVisitor {
 public:
  MirVisitor() = default;
  MirVisitor(const MirVisitor&) = default;
  MirVisitor(MirVisitor&&) = delete;
  auto operator=(const MirVisitor&) -> MirVisitor& = default;
  auto operator=(MirVisitor&&) -> MirVisitor& = delete;
  virtual ~MirVisitor() = default;

  virtual void Visit(const LiteralExpression&) = 0;
  virtual void Visit(const IdentifierExpression&) = 0;
  virtual void Visit(const UnaryExpression&) = 0;
  virtual void Visit(const BinaryExpression&) = 0;
  virtual void Visit(const TernaryExpression&) = 0;
  virtual void Visit(const AssignmentExpression&) = 0;
  virtual void Visit(const ConversionExpression&) = 0;
  virtual void Visit(const SystemCallExpression&) = 0;

  virtual void Visit(const VariableDeclarationStatement&) = 0;
  virtual void Visit(const AssignStatement&) = 0;
  virtual void Visit(const ExpressionStatement&) = 0;
  virtual void Visit(const WaitEventStatement&) = 0;
  virtual void Visit(const DelayStatement&) = 0;
  virtual void Visit(const ConditionalStatement&) = 0;
  virtual void Visit(const WhileStatement&) = 0;
  virtual void Visit(const DoWhileStatement&) = 0;
  virtual void Visit(const BreakStatement&) = 0;
  virtual void Visit(const ContinueStatement&) = 0;
  virtual void Visit(const BlockStatement&) = 0;
};

}  // namespace lyra::mir
