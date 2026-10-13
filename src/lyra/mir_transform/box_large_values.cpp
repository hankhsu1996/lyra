#include "lyra/mir_transform/box_large_values.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

auto BoxOf(const mir::CompilationUnit& unit, mir::TypeId pointee)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = pointee, .ownership = mir::PointerOwnership::kUnique}});
}

// Whether a call enters a body this program compiles, whose parameters the
// same rule boxes: a subroutine, a method, a closure, the constructor of an
// object of this program's classes.
// A runtime entry and a foreign one take what the call states as it is, and so
// does a struct's method, which the library reaches through the struct type's
// table by a convention of its own.
auto EntersACompiledBody(
    const mir::CompilationUnit& unit, const mir::CallExpr& call,
    mir::TypeId result) -> bool {
  return std::visit(
      Overloaded{
          [](const mir::Direct& direct) {
            return !std::holds_alternative<support::BuiltinFn>(direct.target) &&
                   !std::holds_alternative<mir::ForeignSymbolTarget>(
                       direct.target) &&
                   !std::holds_alternative<mir::StructMethodTarget>(
                       direct.target);
          },
          [](const mir::Indirect&) { return true; },
          [](const mir::Virtual&) { return true; },
          [&](const mir::Construct&) {
            return unit.types.Get(result).Is<mir::ObjectType>();
          }},
      call.callee);
}

auto BoxBuiltFrom(BlockRebuild& rebuild, mir::TypeId box, mir::ExprId value)
    -> mir::ExprId {
  return rebuild.Add(mir::MakeNewExpr(value, box));
}

// A subroutine or a method answering a large value answers its box, which
// every call of it, all of which this program compiles, reads through. A
// closure is entered by the runtime by the protocol its type states, and
// what an execution completes with lands in storage its awaiter is handed, so
// neither changes what it answers.
auto AnswerInABox(const mir::CompilationUnit& unit, mir::CallableCode code)
    -> mir::CallableCode {
  if (mir::IsTooLargeForAFrame(unit, code.result_type)) {
    code.result_type = BoxOf(unit, code.result_type);
  }
  return code;
}

// Whether a body answers the box of a large value, which each of its returns
// then builds.
auto AnswersABox(const mir::CompilationUnit& unit, mir::TypeId answer) -> bool {
  const auto* pointer = unit.types.Get(answer).As<mir::PointerType>();
  return pointer != nullptr &&
         pointer->ownership == mir::PointerOwnership::kUnique &&
         mir::IsTooLargeForAFrame(unit, pointer->pointee);
}

// Whether any node of `block`, or of a block nested in it, is a value of a
// large type.
auto HoldsALargeValue(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return mir::IsTooLargeForAFrame(unit, expr.type);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return HoldsALargeValue(unit, child);
         });
}

// Whether the node `id` of `block`, read as a value, makes one -- as an
// operation, a construction or a call does -- rather than reading one where
// it lies, as a name, a dereference, a field and a part of storage do.
auto MakesAValue(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId id)
    -> bool {
  const auto names = [](const auto&) { return false; };
  const auto makes = [](const auto&) { return true; };
  return std::visit(
      Overloaded{
          [&](const mir::StringLiteral& e) { return names(e); },
          [&](const mir::NullLiteral& e) { return names(e); },
          [&](const mir::MachineBoolLiteral& e) { return names(e); },
          [&](const mir::MachineIntLiteral& e) { return names(e); },
          [&](const mir::MachineFloatLiteral& e) { return names(e); },
          [&](const mir::ReferenceExpr& e) { return names(e); },
          [&](const mir::DerefExpr& e) { return names(e); },
          [&](const mir::AddressOfExpr& e) { return names(e); },
          [&](const mir::BorrowExpr& e) { return names(e); },
          [&](const mir::FieldAccessExpr& e) { return names(e); },
          [&](const mir::NewExpr& e) { return names(e); },
          // A large one chooses its value's box, and is read through it.
          [&](const mir::ConditionalExpr& e) { return names(e); },
          // A call into a body answering a large value answers its box, and
          // is read through it.
          [&](const mir::CallExpr& call) {
            return !mir::ReadsWhereItLies(unit.types, block, id) &&
                   !EntersACompiledBody(unit, call, block.exprs.Get(id).type);
          },
          // What an execution completes with lands in storage its awaiter is
          // handed, and is read there.
          [&](const mir::AwaitExpr& e) { return names(e); },
          [&](const mir::UnaryExpr& e) { return makes(e); },
          [&](const mir::BinaryExpr& e) { return makes(e); },
          [&](const mir::CastExpr& e) { return makes(e); },
          [&](const mir::DynamicCastExpr& e) { return makes(e); },
          [&](const mir::BlockExpr& e) { return makes(e); },
          [&](const mir::AssignExpr& e) { return makes(e); },
          [&](const mir::MoveExpr& e) { return makes(e); },
          [&](const mir::ClosureExpr& e) { return makes(e); },
          [&](const mir::CompositeExpr& e) { return makes(e); },
          [&](const mir::WaitExpr& e) { return makes(e); },
          [&](const mir::VectorGetExpr& e) { return makes(e); }},
      block.exprs.Get(id).data);
}

// `operand`, a node of the block `rebuild` builds from, carried over as a
// value read: one of a large type it makes is made in a box and read through
// it, so no frame holds it.
auto CarriedValue(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild,
    mir::ExprId operand) -> mir::ExprId {
  const mir::TypeId type = rebuild.From().exprs.Get(operand).type;
  const mir::ExprId carried = rebuild.Expr(operand);
  if (!mir::IsTooLargeForAFrame(unit, type) ||
      !MakesAValue(unit, rebuild.From(), operand)) {
    return carried;
  }
  return rebuild.Add(
      mir::MakeDerefExpr(
          BoxBuiltFrom(rebuild, BoxOf(unit, type), carried), type));
}

// `argument`, carried over to a body this program compiles: in a box built
// from it where its type is large, which is how that body takes it.
auto CarriedArgument(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild,
    mir::ExprId argument) -> mir::ExprId {
  const mir::TypeId type = rebuild.From().exprs.Get(argument).type;
  if (!mir::IsTooLargeForAFrame(unit, type)) {
    return CarriedValue(unit, rebuild, argument);
  }
  return BoxBuiltFrom(rebuild, BoxOf(unit, type), rebuild.Expr(argument));
}

// The box each field of a closure that captures a value of a large type holds
// it through, by closure and field, and nothing for a field holding its capture
// as it is. Read from the unit before any closure is rewritten, so a body
// building a closure and the closure's own body agree.
using CaptureBoxes = std::vector<std::vector<std::optional<mir::TypeId>>>;

auto CaptureBoxesOf(const mir::CompilationUnit& unit) -> CaptureBoxes {
  CaptureBoxes boxes(unit.closures.size());
  for (const mir::ClosureId id : unit.closures.Ids()) {
    const mir::ClosureDecl& closure = unit.closures.Get(id);
    std::vector<std::optional<mir::TypeId>>& fields = boxes.at(id.value);
    fields.resize(closure.fields.size());
    for (const mir::FieldId field : closure.fields.Ids()) {
      const mir::TypeId type = closure.fields.Get(field).type;
      if (mir::IsTooLargeForAFrame(unit, type)) {
        fields.at(field.value) = BoxOf(unit, type);
      }
    }
  }
  return boxes;
}

// `target`, what a write lands on, carried over: each part it is reached
// through names where that part lies rather than being read, so the chain is
// carried as it stands, and only what selects a part is read as a value.
auto CarriedTarget(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild, mir::ExprId target)
    -> mir::ExprId {
  const mir::Expr& expr = rebuild.From().exprs.Get(target);
  const auto* part = std::get_if<mir::CallExpr>(&expr.data);
  const auto* direct =
      part != nullptr ? std::get_if<mir::Direct>(&part->callee) : nullptr;
  if (direct == nullptr || !direct->receiver.has_value() ||
      !mir::SelectionOf(*part).has_value()) {
    return rebuild.Expr(target);
  }
  mir::Direct callee = *direct;
  callee.receiver = CarriedTarget(unit, rebuild, *direct->receiver);
  mir::CallExpr carried{
      .callee = std::move(callee), .arguments = part->arguments};
  for (mir::ExprId& selector : carried.arguments) {
    selector = CarriedValue(unit, rebuild, selector);
  }
  return rebuild.Add(mir::Expr{.data = std::move(carried), .type = expr.type});
}

// A node carried over with each operand it reads as a value carried by
// `CarriedValue`, and every operand naming a place carried as it is. A node
// whose operands are all places, or that reads none, is left to the rebuild.
// A capture of a large value is built into its box and reached through it.
auto CarriedNode(
    const mir::CompilationUnit& unit, const CaptureBoxes& captures,
    BlockRebuild& rebuild, const mir::Expr& expr) -> std::optional<mir::Expr> {
  const auto value = [&](mir::ExprId operand) {
    return CarriedValue(unit, rebuild, operand);
  };
  const auto node = [&](mir::ExprData data) -> std::optional<mir::Expr> {
    return mir::Expr{.data = std::move(data), .type = expr.type};
  };
  const auto left = [](const auto&) -> std::optional<mir::Expr> {
    return std::nullopt;
  };
  return std::visit(
      Overloaded{
          [&](const mir::UnaryExpr& e) {
            return node(
                mir::UnaryExpr{.op = e.op, .operand = value(e.operand)});
          },
          [&](const mir::BinaryExpr& e) {
            return node(
                mir::BinaryExpr{
                    .op = e.op, .lhs = value(e.lhs), .rhs = value(e.rhs)});
          },
          [&](const mir::CastExpr& e) {
            return node(mir::CastExpr{.operand = value(e.operand)});
          },
          [&](const mir::DynamicCastExpr& e) {
            mir::DynamicCastExpr carried = e;
            carried.operand = value(e.operand);
            return node(std::move(carried));
          },
          // A value of a large type a condition chooses is chosen as its box,
          // each arm made into a box of its own, so what the two ways join at
          // holds a pointer.
          [&](const mir::ConditionalExpr& e) {
            if (!mir::IsTooLargeForAFrame(unit, expr.type)) {
              return node(
                  mir::ConditionalExpr{
                      .condition = value(e.condition),
                      .then_value = value(e.then_value),
                      .else_value = value(e.else_value)});
            }
            const mir::TypeId box = BoxOf(unit, expr.type);
            const mir::ExprId chosen = rebuild.Add(
                mir::Expr{
                    .data =
                        mir::ConditionalExpr{
                            .condition = value(e.condition),
                            .then_value = BoxBuiltFrom(
                                rebuild, box, rebuild.Expr(e.then_value)),
                            .else_value = BoxBuiltFrom(
                                rebuild, box, rebuild.Expr(e.else_value))},
                    .type = box});
            return std::optional<mir::Expr>{
                mir::MakeDerefExpr(chosen, expr.type)};
          },
          [&](const mir::AssignExpr& e) {
            mir::AssignExpr carried = e;
            carried.target = CarriedTarget(unit, rebuild, e.target);
            carried.value = value(e.value);
            return node(std::move(carried));
          },
          [&](const mir::CallExpr& e) {
            mir::CallExpr carried = e;
            // A receiver is read as a value where a call stands as one, so one
            // made is made in a box like any operand.
            std::visit(
                Overloaded{
                    [&](mir::Direct& callee) {
                      if (callee.receiver.has_value()) {
                        callee.receiver = value(*callee.receiver);
                      }
                    },
                    [&](mir::Indirect& callee) {
                      callee.code = rebuild.Expr(callee.code);
                    },
                    [](mir::Construct&) {},
                    [&](mir::Virtual& callee) {
                      callee.receiver = value(callee.receiver);
                    }},
                carried.callee);
            const bool enters_body = EntersACompiledBody(unit, e, expr.type);
            for (mir::ExprId& argument : carried.arguments) {
              argument = enters_body ? CarriedArgument(unit, rebuild, argument)
                                     : value(argument);
            }
            // A body answering a large value answers its box, which the call
            // is then read through.
            if (!enters_body || !mir::IsTooLargeForAFrame(unit, expr.type)) {
              return node(std::move(carried));
            }
            return std::optional<mir::Expr>{mir::MakeDerefExpr(
                rebuild.Add(
                    mir::Expr{
                        .data = std::move(carried),
                        .type = BoxOf(unit, expr.type)}),
                expr.type)};
          },
          [&](const mir::AwaitExpr& e) { return left(e); },
          // A large value made to be borrowed is made in a box, which the
          // reference then refers into.
          [&](const mir::BorrowExpr& e) {
            return node(mir::BorrowExpr{.operand = value(e.operand)});
          },
          [&](const mir::CompositeExpr& e) {
            mir::CompositeExpr carried = e;
            for (mir::ExprId& part : carried.parts) {
              part = value(part);
            }
            return node(std::move(carried));
          },
          [&](const mir::ClosureExpr& e) {
            mir::ClosureExpr carried = e;
            for (mir::FieldInit& init : carried.field_inits) {
              const std::optional<mir::TypeId> box =
                  captures.at(e.closure.value).at(init.target.value);
              init.value =
                  box.has_value()
                      ? BoxBuiltFrom(rebuild, *box, rebuild.Expr(init.value))
                      : value(init.value);
            }
            return node(std::move(carried));
          },
          [&](const mir::FieldAccessExpr& e) -> std::optional<mir::Expr> {
            const auto* capture =
                std::get_if<mir::ClosureFieldTarget>(&e.field);
            const std::optional<mir::TypeId> box =
                capture == nullptr
                    ? std::nullopt
                    : captures.at(capture->owner.value).at(capture->slot.value);
            if (!box.has_value()) {
              return std::nullopt;
            }
            const mir::ExprId held = rebuild.Add(
                mir::MakeFieldAccessExpr(
                    rebuild.Expr(e.receiver), e.field, *box));
            return mir::MakeDerefExpr(held, expr.type);
          },
          [&](const mir::StringLiteral& e) { return left(e); },
          [&](const mir::NullLiteral& e) { return left(e); },
          [&](const mir::MachineBoolLiteral& e) { return left(e); },
          [&](const mir::MachineIntLiteral& e) { return left(e); },
          [&](const mir::MachineFloatLiteral& e) { return left(e); },
          [&](const mir::ReferenceExpr& e) { return left(e); },
          [&](const mir::DerefExpr& e) { return left(e); },
          [&](const mir::AddressOfExpr& e) { return left(e); },
          [&](const mir::NewExpr& e) { return left(e); },
          [&](const mir::BlockExpr& e) { return left(e); },
          [&](const mir::MoveExpr& e) { return left(e); },
          [&](const mir::WaitExpr& e) { return left(e); },
          [&](const mir::VectorGetExpr& e) { return left(e); }},
      expr.data);
}

// A body whose large locals and parameters are held through a box: a local is
// declared as the box its value is built in, a parameter arrives as one, every
// name of either reaches the value through the box, and an argument of a large
// type is handed to a body this program compiles in a box built from it.
// `base_args`, nodes of the body no statement reaches, are carried with it and
// are such arguments: a constructor's to its base's.
auto BoxedCode(
    const mir::CompilationUnit& unit, const CaptureBoxes& captures,
    mir::CallableCode code, std::span<mir::ExprId> base_args = {})
    -> mir::CallableCode {
  std::vector<std::optional<mir::TypeId>> boxed(code.locals.size());
  for (const mir::LocalId id : code.locals.Ids()) {
    const mir::TypeId type = code.locals.Get(id).type;
    if (mir::IsTooLargeForAFrame(unit, type)) {
      boxed.at(id.value) = BoxOf(unit, type);
    }
  }
  const auto box_of = [&](mir::LocalId id) -> std::optional<mir::TypeId> {
    return boxed.at(id.value);
  };
  const bool boxes_a_local = std::ranges::any_of(
      boxed,
      [](const std::optional<mir::TypeId>& box) { return box.has_value(); });
  const bool answers_a_box = AnswersABox(unit, code.result_type);
  if (!boxes_a_local &&
      (!code.body.has_value() || !HoldsALargeValue(unit, *code.body))) {
    return code;
  }
  if (code.body.has_value()) {
    const RebuildRules rules{
        .expr = [&](BlockRebuild& rebuild,
                    const mir::Expr& expr) -> std::optional<mir::Expr> {
          if (const std::optional<mir::LocalId> local =
                  mir::ReferencedLocal(expr.data);
              local.has_value() && box_of(*local).has_value()) {
            const mir::ExprId box =
                rebuild.Add(mir::MakeLocalRefExpr(*local, *box_of(*local)));
            return mir::MakeDerefExpr(box, expr.type);
          }
          return CarriedNode(unit, captures, rebuild, expr);
        },
        .stmt = [&](BlockRebuild& rebuild,
                    const mir::Stmt& stmt) -> std::optional<mir::StmtData> {
          // A body answering its value's box returns a box built from what
          // it answers; any other answer is read as a value.
          if (const auto* returned = std::get_if<mir::ReturnStmt>(&stmt.data);
              returned != nullptr && returned->value.has_value()) {
            if (!answers_a_box) {
              return mir::ReturnStmt{
                  .value = CarriedValue(unit, rebuild, *returned->value)};
            }
            const mir::TypeId type =
                rebuild.From().exprs.Get(*returned->value).type;
            return mir::ReturnStmt{
                .value = BoxBuiltFrom(
                    rebuild, BoxOf(unit, type),
                    rebuild.Expr(*returned->value))};
          }
          // A value a statement makes and discards is made in a box too.
          if (const auto* evaluated = std::get_if<mir::ExprStmt>(&stmt.data)) {
            return mir::ExprStmt{
                .expr = CarriedValue(unit, rebuild, evaluated->expr)};
          }
          const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data);
          if (decl == nullptr || !box_of(decl->target).has_value()) {
            return std::nullopt;
          }
          return mir::LocalDeclStmt{
              .target = decl->target,
              .init = BoxBuiltFrom(
                  rebuild, *box_of(decl->target), rebuild.Expr(decl->init))};
        }};
    BlockRebuild rebuild(*code.body, rules);
    std::vector<mir::StmtId> stmts = rebuild.Stmts(code.body->root_stmts);
    for (mir::ExprId& argument : base_args) {
      argument = CarriedArgument(unit, rebuild, argument);
    }
    code.body = std::move(rebuild).Finish(std::move(stmts));
  }
  base::Arena<mir::LocalDecl, mir::LocalId> locals;
  for (const mir::LocalId id : code.locals.Ids()) {
    locals.Add(
        mir::LocalDecl{.type = box_of(id).value_or(code.locals.Get(id).type)});
  }
  code.locals = std::move(locals);
  return code;
}

}  // namespace

auto BoxLargeValues(mir::CompilationUnit unit) -> mir::CompilationUnit {
  const CaptureBoxes captures = CaptureBoxesOf(unit);
  unit.classes = std::move(unit.classes)
                     .TransformValues([&](mir::ClassId, mir::Class cls) {
                       if (cls.constructor.has_value()) {
                         cls.constructor->code = BoxedCode(
                             unit, captures, std::move(cls.constructor->code),
                             cls.constructor->base_args);
                       }
                       cls.callables =
                           std::move(cls.callables)
                               .TransformValues([&](mir::CallableId,
                                                    mir::CallableDecl decl) {
                                 decl.code = BoxedCode(
                                     unit, captures,
                                     AnswerInABox(unit, std::move(decl.code)));
                                 return decl;
                               });
                       return cls;
                     });
  // A foreign body is entered by the convention its declaration states, which
  // nothing here changes.
  unit.callables =
      std::move(unit.callables)
          .TransformValues([&](mir::CallableId, mir::CallableDecl decl) {
            if (!decl.foreign.has_value()) {
              decl.code = BoxedCode(
                  unit, captures, AnswerInABox(unit, std::move(decl.code)));
            }
            return decl;
          });
  // A struct's method takes the struct by reference and answers by the
  // convention the library calls it through, so what changes is only what its
  // body holds. The size of a value is read from the members of the structs
  // the unit declares, so every method is rewritten before any struct is taken
  // out of the unit to be replaced.
  std::vector<std::vector<mir::StructMethod>> methods(unit.structs.size());
  for (const mir::StructId id : unit.structs.Ids()) {
    for (const mir::StructMethod& method : unit.structs.Get(id).methods) {
      methods.at(id.value).push_back(
          mir::StructMethod{
              .answers = method.answers,
              .code = BoxedCode(unit, captures, method.code)});
    }
  }
  unit.structs =
      std::move(unit.structs)
          .TransformValues([&](mir::StructId id, mir::StructDecl decl) {
            decl.methods = std::move(methods.at(id.value));
            return decl;
          });
  // A capture of a large value is held through its box, as a local is.
  unit.closures =
      std::move(unit.closures)
          .TransformValues([&](mir::ClosureId id, mir::ClosureDecl closure) {
            closure.invoke =
                BoxedCode(unit, captures, std::move(closure.invoke));
            base::Arena<mir::FieldDecl, mir::FieldId> fields;
            for (const mir::FieldId field : closure.fields.Ids()) {
              fields.Add(
                  mir::FieldDecl{
                      .type = captures.at(id.value)
                                  .at(field.value)
                                  .value_or(closure.fields.Get(field).type)});
            }
            closure.fields = std::move(fields);
            return closure;
          });
  return unit;
}

}  // namespace lyra::mir_transform
