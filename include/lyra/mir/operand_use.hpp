#pragma once

#include <concepts>
#include <cstdint>
#include <type_traits>
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

// `T` as const as `Data` is, so one visit serves a node read and a node being
// rebuilt.
template <typename Data, typename T>
using AsConstAs = std::conditional_t<std::is_const_v<Data>, const T, T>;

// Calls `reach` with each node `data` takes a value from, and how it takes
// it, in the order an evaluation of `data` runs them: first to last, an
// assignment's value before its target, as Rust evaluates an assignment and as
// a write with an intra-assignment timing control evaluates its value before
// its target (LRM 10.4.1), and a call's arguments before its callee and the
// object it is entered on, which often stands for storage that ends with the
// full-expression and so runs where it is used. Every one is named in the
// block holding `data` but a block expression's value, which is named in the
// block of its steps and is reached from there. Handed a node being rebuilt,
// `reach` is handed each operand to replace.
template <typename Data>
  requires std::same_as<std::remove_const_t<Data>, ExprData>
void ForEachOperand(Data& data, const auto& reach) {
  using Id = AsConstAs<Data, ExprId>;
  const auto value = [&](Id& operand) { reach(operand, OperandUse::kValue); };
  std::visit(
      Overloaded{
          [](AsConstAs<Data, StringLiteral>&) {},
          [](AsConstAs<Data, NullLiteral>&) {},
          [](AsConstAs<Data, MachineBoolLiteral>&) {},
          [](AsConstAs<Data, MachineIntLiteral>&) {},
          [](AsConstAs<Data, MachineFloatLiteral>&) {},
          [](AsConstAs<Data, ReferenceExpr>&) {},
          [&](AsConstAs<Data, UnaryExpr>& e) { value(e.operand); },
          [&](AsConstAs<Data, BinaryExpr>& e) {
            value(e.lhs);
            value(e.rhs);
          },
          [&](AsConstAs<Data, CastExpr>& e) { value(e.operand); },
          [&](AsConstAs<Data, ConditionalExpr>& e) {
            value(e.condition);
            value(e.then_value);
            value(e.else_value);
          },
          [](AsConstAs<Data, BlockExpr>&) {},
          [&](AsConstAs<Data, AssignExpr>& e) {
            value(e.value);
            reach(e.target, OperandUse::kStorage);
          },
          [&](AsConstAs<Data, CallExpr>& e) {
            for (Id& argument : e.arguments) {
              value(argument);
            }
            std::visit(
                Overloaded{
                    [&](AsConstAs<Data, Direct>& callee) {
                      if (callee.receiver.has_value()) {
                        reach(*callee.receiver, OperandUse::kObject);
                      }
                    },
                    [&](AsConstAs<Data, Indirect>& callee) {
                      value(callee.code);
                    },
                    [](AsConstAs<Data, Construct>&) {},
                    [&](AsConstAs<Data, Virtual>& callee) {
                      reach(callee.receiver, OperandUse::kObject);
                    }},
                e.callee);
          },
          // A placement's arguments are evaluated before the storage is asked
          // for, and that before the initializer ([expr.new]).
          [&](AsConstAs<Data, NewExpr>& e) {
            if (e.placement.has_value()) {
              for (Id& argument : e.placement->arguments) {
                value(argument);
              }
            }
            value(e.initializer);
          },
          [&](AsConstAs<Data, DerefExpr>& e) {
            reach(e.pointer, OperandUse::kPointer);
          },
          [&](AsConstAs<Data, AddressOfExpr>& e) {
            reach(e.operand, OperandUse::kStorage);
          },
          [&](AsConstAs<Data, BorrowExpr>& e) {
            reach(e.operand, OperandUse::kStorage);
          },
          [&](AsConstAs<Data, MoveExpr>& e) {
            reach(e.operand, OperandUse::kRead);
          },
          [&](AsConstAs<Data, CopyExpr>& e) {
            reach(e.place, OperandUse::kRead);
          },
          [&](AsConstAs<Data, DiscriminantExpr>& e) {
            reach(e.tagged, OperandUse::kObject);
          },
          [&](AsConstAs<Data, FieldAccessExpr>& e) {
            reach(e.receiver, OperandUse::kStorage);
          },
          [&](AsConstAs<Data, ClosureExpr>& e) {
            for (AsConstAs<Data, FieldInit>& init : e.field_inits) {
              value(init.value);
            }
          },
          [&](AsConstAs<Data, CompositeExpr>& e) {
            for (Id& part : e.parts) {
              value(part);
            }
          },
          [&](AsConstAs<Data, UnionExpr>& e) { value(e.value); },
          [&](AsConstAs<Data, AwaitExpr>& e) { value(e.execution); },
          [&](AsConstAs<Data, WaitExpr>& e) { value(e.park); },
          [&](AsConstAs<Data, VectorGetExpr>& e) {
            value(e.vector);
            value(e.index);
          }},
      data);
}

// Where the value an operation takes as an operand is made: where the
// operation takes it, or elsewhere -- in the value the operation itself makes,
// as an arm of a choice is, which clang and rustc build into the choice's
// destination; in the storage the operation allocates, as what a
// new-expression initializes is ([expr.new]); or by the runtime, which keeps
// what a suspension waits on for its awaiter.
enum class OperandMadeIn : std::uint8_t {
  kWhereTaken,
  kTheOperationsValue,
  kTheOperationsAllocation,
  kTheRuntime,
};

// Where the value `node` takes as `operand` is made.
[[nodiscard]] inline auto WhereOperandIsMade(
    const ExprData& node, ExprId operand) -> OperandMadeIn {
  if (std::holds_alternative<AwaitExpr>(node) ||
      std::holds_alternative<WaitExpr>(node)) {
    return OperandMadeIn::kTheRuntime;
  }
  if (const auto* chosen = std::get_if<ConditionalExpr>(&node);
      chosen != nullptr && operand != chosen->condition) {
    return OperandMadeIn::kTheOperationsValue;
  }
  if (const auto* allocation = std::get_if<NewExpr>(&node);
      allocation != nullptr && operand == allocation->initializer) {
    return OperandMadeIn::kTheOperationsAllocation;
  }
  return OperandMadeIn::kWhereTaken;
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
