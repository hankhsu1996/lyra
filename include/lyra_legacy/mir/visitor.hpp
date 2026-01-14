#pragma once

namespace lyra::mir {

class Expression;
class ConstantExpression;
class IdentifierExpression;
class EnumValueExpression;
class UnaryExpression;
class BinaryExpression;
class TernaryExpression;
class AssignmentExpression;
class ConversionExpression;
class SystemCallExpression;
class ElementSelectExpression;
class RangeSelectExpression;
class IndexedRangeSelectExpression;
class HierarchicalReferenceExpression;
class ConcatenationExpression;
class ReplicationExpression;
class FunctionCallExpression;
class MemberAccessExpression;
class NewArrayExpression;
class MethodCallExpression;
class UnpackedStructLiteralExpression;
class ArrayLiteralExpression;

class Statement;
class VariableDeclarationStatement;
class AssignStatement;
class ExpressionStatement;
class WaitEventStatement;
class DelayStatement;
class ConditionalStatement;
class WhileStatement;
class DoWhileStatement;
class ForStatement;
class RepeatStatement;
class CaseStatement;
class BreakStatement;
class ContinueStatement;
class BlockStatement;
class ReturnStatement;

class MirVisitor {
 public:
  MirVisitor() = default;
  MirVisitor(const MirVisitor&) = default;
  MirVisitor(MirVisitor&&) = delete;
  auto operator=(const MirVisitor&) -> MirVisitor& = default;
  auto operator=(MirVisitor&&) -> MirVisitor& = delete;
  virtual ~MirVisitor() = default;

  virtual void Visit(const ConstantExpression&) = 0;
  virtual void Visit(const IdentifierExpression&) = 0;
  virtual void Visit(const EnumValueExpression&) = 0;
  virtual void Visit(const UnaryExpression&) = 0;
  virtual void Visit(const BinaryExpression&) = 0;
  virtual void Visit(const TernaryExpression&) = 0;
  virtual void Visit(const AssignmentExpression&) = 0;
  virtual void Visit(const ConversionExpression&) = 0;
  virtual void Visit(const SystemCallExpression&) = 0;
  virtual void Visit(const ElementSelectExpression&) = 0;
  virtual void Visit(const RangeSelectExpression&) = 0;
  virtual void Visit(const IndexedRangeSelectExpression&) = 0;
  virtual void Visit(const HierarchicalReferenceExpression&) = 0;
  virtual void Visit(const ConcatenationExpression&) = 0;
  virtual void Visit(const ReplicationExpression&) = 0;
  virtual void Visit(const FunctionCallExpression&) = 0;
  virtual void Visit(const MemberAccessExpression&) = 0;
  virtual void Visit(const NewArrayExpression&) = 0;
  virtual void Visit(const MethodCallExpression&) = 0;
  virtual void Visit(const UnpackedStructLiteralExpression&) = 0;
  virtual void Visit(const ArrayLiteralExpression&) = 0;

  virtual void Visit(const VariableDeclarationStatement&) = 0;
  virtual void Visit(const AssignStatement&) = 0;
  virtual void Visit(const ExpressionStatement&) = 0;
  virtual void Visit(const WaitEventStatement&) = 0;
  virtual void Visit(const DelayStatement&) = 0;
  virtual void Visit(const ConditionalStatement&) = 0;
  virtual void Visit(const WhileStatement&) = 0;
  virtual void Visit(const DoWhileStatement&) = 0;
  virtual void Visit(const ForStatement&) = 0;
  virtual void Visit(const RepeatStatement&) = 0;
  virtual void Visit(const CaseStatement&) = 0;
  virtual void Visit(const BreakStatement&) = 0;
  virtual void Visit(const ContinueStatement&) = 0;
  virtual void Visit(const BlockStatement&) = 0;
  virtual void Visit(const ReturnStatement&) = 0;
};

}  // namespace lyra::mir
