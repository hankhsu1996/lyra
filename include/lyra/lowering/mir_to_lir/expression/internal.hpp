#pragma once

#include "lyra/lir/context.hpp"

namespace lyra::mir {
class Expression;
class ConstantExpression;
class IdentifierExpression;
class EnumValueExpression;
class UnaryExpression;
class BinaryExpression;
class ConversionExpression;
class ElementSelectExpression;
class RangeSelectExpression;
class IndexedRangeSelectExpression;
class MemberAccessExpression;
class HierarchicalReferenceExpression;
class SystemCallExpression;
class FunctionCallExpression;
class MethodCallExpression;
class ConcatenationExpression;
class ReplicationExpression;
class NewArrayExpression;
class UnpackedStructLiteralExpression;
class ArrayLiteralExpression;
class AssignmentExpression;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_lir {
class LirBuilder;

// Constant lowering
auto LowerConstantExpression(
    const mir::ConstantExpression& constant_expression, LirBuilder& builder)
    -> lir::TempRef;
auto LowerIdentifierExpression(
    const mir::IdentifierExpression& identifier, LirBuilder& builder)
    -> lir::TempRef;
auto LowerEnumValueExpression(
    const mir::EnumValueExpression& enum_val, LirBuilder& builder)
    -> lir::TempRef;

// Operator lowering
auto LowerConversionExpression(
    const mir::ConversionExpression& conversion, LirBuilder& builder)
    -> lir::TempRef;

// Selection lowering
auto LowerElementSelectExpression(
    const mir::ElementSelectExpression& select, LirBuilder& builder)
    -> lir::TempRef;
auto LowerRangeSelectExpression(
    const mir::RangeSelectExpression& range, LirBuilder& builder)
    -> lir::TempRef;
auto LowerIndexedRangeSelectExpression(
    const mir::IndexedRangeSelectExpression& indexed, LirBuilder& builder)
    -> lir::TempRef;
auto LowerMemberAccessExpression(
    const mir::MemberAccessExpression& member, LirBuilder& builder)
    -> lir::TempRef;
auto LowerHierarchicalReferenceExpression(
    const mir::HierarchicalReferenceExpression& hier_ref, LirBuilder& builder)
    -> lir::TempRef;

// Call lowering
auto LowerSystemCallExpression(
    const mir::SystemCallExpression& system_call, LirBuilder& builder)
    -> lir::TempRef;
auto LowerFunctionCallExpression(
    const mir::FunctionCallExpression& call, LirBuilder& builder)
    -> lir::TempRef;
auto LowerMethodCallExpression(
    const mir::MethodCallExpression& mc, LirBuilder& builder) -> lir::TempRef;

// Aggregate lowering
auto LowerConcatenationExpression(
    const mir::ConcatenationExpression& concat, LirBuilder& builder)
    -> lir::TempRef;
auto LowerReplicationExpression(
    const mir::ReplicationExpression& rep, LirBuilder& builder) -> lir::TempRef;
auto LowerNewArrayExpression(
    const mir::NewArrayExpression& new_arr, LirBuilder& builder)
    -> lir::TempRef;
auto LowerUnpackedStructLiteralExpression(
    const mir::UnpackedStructLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef;
auto LowerArrayLiteralExpression(
    const mir::ArrayLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef;

// Assignment lowering
auto LowerAssignmentExpression(
    const mir::AssignmentExpression& assignment, LirBuilder& builder)
    -> lir::TempRef;

}  // namespace lyra::lowering::mir_to_lir
