#pragma once

#include <cstdint>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

// How an operation takes an operand, as clang's Sema decides whether an
// operand undergoes the lvalue-to-rvalue conversion and rustc's builder
// whether it lowers one as an operand or as a place.
enum class OperandUse : std::uint8_t {
  // As a value: a place stands here only under a read.
  kValue,
  // As the storage itself, which a read would replace with a copy: what a
  // write lands in, what a borrow or an address-of takes, the object a field
  // is reached on.
  kStorage,
  // What a read or a move takes: a place holding a value.
  kRead,
  // The object a call is entered on: named where it lies, as C++ binds an
  // implicit object argument, or already read. An entry taking it by value,
  // as a Rust method taking `self` does, is handed it read by each target's
  // realization of the call, so either stands here.
  kObject,
  // The pointer a dereference opens.
  kPointer,
};

// Calls `reach` with each node `data` takes a value from, and how it takes
// it. Every one is named in the block holding `data` but a block expression's
// value, which is named in the block of its steps and is reached from there.
void ForEachOperand(const ExprData& data, const auto& reach) {
  const auto value = [&](ExprId operand) {
    reach(operand, OperandUse::kValue);
  };
  std::visit(
      Overloaded{
          [](const StringLiteral&) {},
          [](const NullLiteral&) {},
          [](const MachineBoolLiteral&) {},
          [](const MachineIntLiteral&) {},
          [](const MachineFloatLiteral&) {},
          [](const ReferenceExpr&) {},
          [&](const UnaryExpr& e) { value(e.operand); },
          [&](const BinaryExpr& e) {
            value(e.lhs);
            value(e.rhs);
          },
          [&](const CastExpr& e) { value(e.operand); },
          [&](const DynamicCastExpr& e) { value(e.operand); },
          [&](const ConditionalExpr& e) {
            value(e.condition);
            value(e.then_value);
            value(e.else_value);
          },
          [](const BlockExpr&) {},
          [&](const AssignExpr& e) {
            reach(e.target, OperandUse::kStorage);
            value(e.value);
          },
          [&](const CallExpr& e) {
            std::visit(
                Overloaded{
                    [&](const Direct& callee) {
                      if (callee.receiver.has_value()) {
                        reach(*callee.receiver, OperandUse::kObject);
                      }
                    },
                    [&](const Indirect& callee) { value(callee.code); },
                    [](const Construct&) {},
                    [&](const Virtual& callee) {
                      reach(callee.receiver, OperandUse::kObject);
                    }},
                e.callee);
            for (const ExprId argument : e.arguments) {
              value(argument);
            }
          },
          [&](const NewExpr& e) { value(e.initializer); },
          [&](const DerefExpr& e) { reach(e.pointer, OperandUse::kPointer); },
          [&](const AddressOfExpr& e) {
            reach(e.operand, OperandUse::kStorage);
          },
          [&](const BorrowExpr& e) { reach(e.operand, OperandUse::kStorage); },
          [&](const MoveExpr& e) { reach(e.operand, OperandUse::kRead); },
          [&](const CopyExpr& e) { reach(e.place, OperandUse::kRead); },
          [&](const DiscriminantExpr& e) {
            reach(e.tagged, OperandUse::kObject);
          },
          [&](const FieldAccessExpr& e) {
            reach(e.receiver, OperandUse::kStorage);
          },
          [&](const ClosureExpr& e) {
            for (const FieldInit& init : e.field_inits) {
              value(init.value);
            }
          },
          [&](const CompositeExpr& e) {
            for (const ExprId part : e.parts) {
              value(part);
            }
          },
          [&](const UnionExpr& e) { value(e.value); },
          [&](const AwaitExpr& e) { value(e.execution); },
          [&](const WaitExpr& e) { value(e.park); },
          [&](const VectorGetExpr& e) {
            value(e.vector);
            value(e.index);
          }},
      data);
}

// The same, with how `expr` takes each operand as it is taken where the node
// stands: a call that binds a reference takes every operand as the storage it
// refers to, as a C++ reference parameter binds an lvalue without reading it.
void ForEachOperandAsTaken(
    const TypePool& types, const Expr& expr, const auto& reach) {
  const auto* call = std::get_if<CallExpr>(&expr.data);
  const bool binds_storage =
      call != nullptr && BindsReference(types, *call, expr.type);
  ForEachOperand(expr.data, [&](ExprId operand, OperandUse use) {
    reach(operand, binds_storage ? OperandUse::kStorage : use);
  });
}

}  // namespace lyra::mir
