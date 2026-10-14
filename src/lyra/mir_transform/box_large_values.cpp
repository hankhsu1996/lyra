#include "lyra/mir_transform/box_large_values.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
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

// Whether the node `id` of `block`, read as a value, makes one rather than
// reading one where it lies. A conditional is the one that makes a value and
// is still not made in a box here: one of a large type is rewritten to choose
// its value's box, and is read through it.
auto MakesAValue(const mir::Block& block, mir::ExprId id) -> bool {
  return !mir::ValueLiesInStorage(block, id) &&
         !std::holds_alternative<mir::ConditionalExpr>(
             block.exprs.Get(id).data);
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

// The rewrite of one body whose large locals and parameters are held through a
// box: a local is declared as the box its value is built in, a parameter
// arrives as one, every name of either reaches the value through the box, and
// an argument of a large type is handed to a body this program compiles in a
// box built from it. It holds what it learns of the body once, and has a step
// for each kind of node, statement and operand, each handed the block it
// builds.
class BodyBoxing {
 public:
  BodyBoxing(
      const mir::CompilationUnit& unit, const CaptureBoxes& captures,
      const mir::CallableCode& code)
      : unit_(&unit),
        capture_boxes_(&captures),
        code_(&code),
        local_boxes_(code.locals.size()),
        completes_(unit.types.Get(code.result_type).Is<mir::CoroutineType>()) {
    for (const mir::LocalId id : code.locals.Ids()) {
      const mir::TypeId type = code.locals.Get(id).type;
      if (mir::IsTooLargeForAFrame(unit, type)) {
        local_boxes_.at(id.value) = BoxOf(unit, type);
      }
    }
  }

  // The body rewritten, read where it lies, with `base_args` carried along:
  // nodes of the body no statement reaches, which a constructor enters its
  // base's with, a body this program compiles.
  auto Rewritten(std::vector<mir::ExprId> base_args) -> mir::ConstructorDecl {
    std::optional<mir::Block> body = RewrittenBody(base_args);
    return mir::ConstructorDecl{
        .code =
            mir::CallableCode{
                .params = code_->params,
                .receiver = code_->receiver,
                .result_type = code_->result_type,
                .locals = BoxedLocals(),
                .named_locals = code_->named_locals,
                .body = std::move(body)},
        .base_args = std::move(base_args)};
  }

  // Whether the rewrite changes the body at all.
  [[nodiscard]] auto ChangesAnything() const -> bool {
    return std::ranges::any_of(
               local_boxes_,
               [](const std::optional<mir::TypeId>& box) {
                 return box.has_value();
               }) ||
           (code_->body.has_value() && HoldsALargeValue(*unit_, *code_->body));
  }

 private:
  [[nodiscard]] auto BoxedLocals() const
      -> base::Arena<mir::LocalDecl, mir::LocalId> {
    base::Arena<mir::LocalDecl, mir::LocalId> locals;
    for (const mir::LocalId id : code_->locals.Ids()) {
      locals.Add(
          mir::LocalDecl{
              .type = local_boxes_.at(id.value).value_or(
                  code_->locals.Get(id).type)});
    }
    return locals;
  }

  auto RewrittenBody(std::vector<mir::ExprId>& base_args)
      -> std::optional<mir::Block> {
    if (!code_->body.has_value()) {
      return std::nullopt;
    }
    const RebuildRules rules{
        .expr = [this](
                    BlockRebuild& rebuild,
                    const mir::Expr& expr) { return Node(rebuild, expr); },
        .stmt =
            [this](BlockRebuild& rebuild, const mir::Stmt& stmt) {
              return Statement(rebuild, stmt);
            }};
    BlockRebuild rebuild(*code_->body, rules);
    std::vector<mir::StmtId> stmts = rebuild.Stmts(code_->body->root_stmts);
    for (mir::ExprId& argument : base_args) {
      argument = Argument(rebuild, argument);
    }
    return std::move(rebuild).Finish(std::move(stmts));
  }

  // Whether `operand`, a node of the block `rebuild` builds from, makes a
  // value of a large type, which is then made in a box.
  [[nodiscard]] auto MadeInABox(
      const BlockRebuild& rebuild, mir::ExprId operand) const -> bool {
    return mir::IsTooLargeForAFrame(
               *unit_, rebuild.From().exprs.Get(operand).type) &&
           MakesAValue(rebuild.From(), operand);
  }

  // `operand`, a node of the block `rebuild` builds from, carried over where
  // it is held: one of a large type it makes is made in a box, and what is
  // carried is that box's storage, so no frame holds it.
  auto Held(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(operand).type;
    const mir::ExprId carried = rebuild.Expr(operand);
    if (!MadeInABox(rebuild, operand)) {
      return carried;
    }
    return rebuild.Add(
        mir::MakeDerefExpr(
            BoxBuiltFrom(rebuild, BoxOf(*unit_, type), carried), type));
  }

  // `operand` carried over as a value read: one made in a box is read out of
  // it where it lies.
  auto Value(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(operand).type;
    const mir::ExprId held = Held(rebuild, operand);
    if (!MadeInABox(rebuild, operand)) {
      return held;
    }
    return rebuild.Add(mir::MakeCopyExpr(held, type));
  }

  // `argument`, carried over to a body this program compiles: in a box built
  // from it where its type is large, which is how that body takes it.
  auto Argument(BlockRebuild& rebuild, mir::ExprId argument) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(argument).type;
    if (!mir::IsTooLargeForAFrame(*unit_, type)) {
      return Value(rebuild, argument);
    }
    return BoxBuiltFrom(rebuild, BoxOf(*unit_, type), rebuild.Expr(argument));
  }

  // `target`, what a write lands on, carried over: each part it is reached
  // through names where that part lies rather than being read, so the chain
  // is carried as it stands, and only what selects a part is read as a value.
  auto Target(BlockRebuild& rebuild, mir::ExprId target) -> mir::ExprId {
    const std::optional<mir::ExprId> whole =
        mir::SelectedFrom(rebuild.From(), target);
    if (!whole.has_value()) {
      return rebuild.Expr(target);
    }
    const mir::Expr& expr = rebuild.From().exprs.Get(target);
    mir::CallExpr carried = std::get<mir::CallExpr>(expr.data);
    std::get<mir::Direct>(carried.callee).receiver = Target(rebuild, *whole);
    for (mir::ExprId& selector : carried.arguments) {
      selector = Value(rebuild, selector);
    }
    return rebuild.Add(
        mir::Expr{.data = std::move(carried), .type = expr.type});
  }

  // A node carried over with each operand it reads as a value carried by
  // `Value`, and every operand naming a place carried as it is. A node whose
  // operands are all places, or that reads none, is left to the rebuild. A
  // name of a boxed local, and a capture of a large value, is reached through
  // its box.
  auto Node(BlockRebuild& rebuild, const mir::Expr& expr)
      -> std::optional<mir::Expr> {
    if (const std::optional<mir::LocalId> local =
            mir::ReferencedLocal(expr.data);
        local.has_value() && local_boxes_.at(local->value).has_value()) {
      const mir::ExprId box = rebuild.Add(
          mir::MakeLocalRefExpr(*local, *local_boxes_.at(local->value)));
      return mir::MakeDerefExpr(box, expr.type);
    }
    const auto value = [&](mir::ExprId operand) {
      return Value(rebuild, operand);
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
            // A value of a large type a condition chooses is chosen as its
            // box, each arm made into a box of its own, so what the two ways
            // join at holds a pointer.
            [&](const mir::ConditionalExpr& e) {
              if (!mir::IsTooLargeForAFrame(*unit_, expr.type)) {
                return node(
                    mir::ConditionalExpr{
                        .condition = value(e.condition),
                        .then_value = value(e.then_value),
                        .else_value = value(e.else_value)});
              }
              const mir::TypeId box = BoxOf(*unit_, expr.type);
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
              return std::optional<mir::Expr>{mir::MakeCopyExpr(
                  rebuild.Add(mir::MakeDerefExpr(chosen, expr.type)),
                  expr.type)};
            },
            [&](const mir::AssignExpr& e) {
              mir::AssignExpr carried = e;
              carried.target = Target(rebuild, e.target);
              carried.value = value(e.value);
              return node(std::move(carried));
            },
            [&](const mir::CallExpr& e) {
              mir::CallExpr carried = e;
              // The object a call is entered on is bound where it lies, so one
              // made of a large type is made in a box and entered there.
              std::visit(
                  Overloaded{
                      [&](mir::Direct& callee) {
                        if (callee.receiver.has_value()) {
                          callee.receiver = Held(rebuild, *callee.receiver);
                        }
                      },
                      [&](mir::Indirect& callee) {
                        callee.code = rebuild.Expr(callee.code);
                      },
                      [](mir::Construct&) {},
                      [&](mir::Virtual& callee) {
                        callee.receiver = Held(rebuild, callee.receiver);
                      }},
                  carried.callee);
              const bool enters_body =
                  EntersACompiledBody(*unit_, e, expr.type);
              for (mir::ExprId& argument : carried.arguments) {
                argument =
                    enters_body ? Argument(rebuild, argument) : value(argument);
              }
              return node(std::move(carried));
            },
            [&](const mir::AwaitExpr& e) { return left(e); },
            // A large value made to be borrowed is made in a box, which the
            // reference then refers into.
            [&](const mir::BorrowExpr& e) {
              return node(mir::BorrowExpr{.operand = Held(rebuild, e.operand)});
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
                    capture_boxes_->at(e.closure.value).at(init.target.value);
                init.value =
                    box.has_value()
                        ? BoxBuiltFrom(rebuild, *box, rebuild.Expr(init.value))
                        : value(init.value);
              }
              return node(std::move(carried));
            },
            // A field of a value a node makes is a field of that value's
            // box, as rustc's `as_place` holds such a value in a temporary.
            [&](const mir::FieldAccessExpr& e) -> std::optional<mir::Expr> {
              const auto* capture =
                  std::get_if<mir::ClosureFieldTarget>(&e.field);
              const std::optional<mir::TypeId> box =
                  capture == nullptr ? std::nullopt
                                     : capture_boxes_->at(capture->owner.value)
                                           .at(capture->slot.value);
              if (!box.has_value()) {
                return mir::MakeFieldAccessExpr(
                    Held(rebuild, e.receiver), e.field, expr.type);
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
            [&](const mir::CopyExpr& e) { return left(e); },
            [&](const mir::WaitExpr& e) { return left(e); },
            [&](const mir::VectorGetExpr& e) { return left(e); }},
        expr.data);
  }

  auto Statement(BlockRebuild& rebuild, const mir::Stmt& stmt)
      -> std::optional<mir::StmtData> {
    // A body makes what it answers in the place its caller takes it from; an
    // execution's answer is read as a value and stored into its completion.
    if (const auto* returned = std::get_if<mir::ReturnStmt>(&stmt.data);
        returned != nullptr && returned->value.has_value()) {
      return mir::ReturnStmt{
          .value = completes_ ? Value(rebuild, *returned->value)
                              : rebuild.Expr(*returned->value)};
    }
    // A value a statement makes and discards is made in a box too.
    if (const auto* evaluated = std::get_if<mir::ExprStmt>(&stmt.data)) {
      return mir::ExprStmt{.expr = Value(rebuild, evaluated->expr)};
    }
    const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data);
    if (decl == nullptr || !local_boxes_.at(decl->target.value).has_value()) {
      return std::nullopt;
    }
    return mir::LocalDeclStmt{
        .target = decl->target,
        .init = BoxBuiltFrom(
            rebuild, *local_boxes_.at(decl->target.value),
            rebuild.Expr(decl->init))};
  }

  const mir::CompilationUnit* unit_;
  const CaptureBoxes* capture_boxes_;
  const mir::CallableCode* code_;
  // The box each local of a large type is held through, none for every other.
  std::vector<std::optional<mir::TypeId>> local_boxes_;
  // Whether the body is an execution, which answers through its completion.
  bool completes_;
};

// A constructor with its large values held through boxes; one with nothing to
// box is handed back as it is.
auto BoxedConstructor(
    const mir::CompilationUnit& unit, const CaptureBoxes& captures,
    mir::ConstructorDecl constructor) -> mir::ConstructorDecl {
  BodyBoxing boxing(unit, captures, constructor.code);
  if (!boxing.ChangesAnything()) {
    return constructor;
  }
  return boxing.Rewritten(std::move(constructor.base_args));
}

// `code` with its large values held through boxes, as a constructor's that
// enters no base.
auto BoxedCode(
    const mir::CompilationUnit& unit, const CaptureBoxes& captures,
    mir::CallableCode code) -> mir::CallableCode {
  return BoxedConstructor(
             unit, captures,
             mir::ConstructorDecl{.code = std::move(code), .base_args = {}})
      .code;
}

}  // namespace

auto BoxLargeValues(mir::CompilationUnit unit) -> mir::CompilationUnit {
  const CaptureBoxes captures = CaptureBoxesOf(unit);
  unit.classes =
      std::move(unit.classes)
          .TransformValues([&](mir::ClassId, mir::Class cls) {
            if (cls.constructor.has_value()) {
              cls.constructor =
                  BoxedConstructor(unit, captures, *std::move(cls.constructor));
            }
            cls.callables = std::move(cls.callables)
                                .TransformValues([&](mir::CallableId,
                                                     mir::CallableDecl decl) {
                                  decl.code = BoxedCode(
                                      unit, captures, std::move(decl.code));
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
              decl.code = BoxedCode(unit, captures, std::move(decl.code));
            }
            return decl;
          });
  // The size of a value is read from the members of the structs the unit
  // declares, so every struct method is rewritten before any struct is taken
  // out of the unit to be replaced.
  std::vector<std::vector<mir::CallableCode>> rewritten(unit.structs.size());
  for (const mir::StructId id : unit.structs.Ids()) {
    for (const mir::StructMethod& method : unit.structs.Get(id).methods) {
      rewritten.at(id.value).push_back(BoxedCode(unit, captures, method.code));
    }
  }
  unit.structs =
      std::move(unit.structs)
          .TransformValues([&](mir::StructId id, mir::StructDecl decl) {
            for (std::size_t i = 0; i < decl.methods.size(); ++i) {
              decl.methods.at(i).code = std::move(rewritten.at(id.value).at(i));
            }
            return decl;
          });
  for (mir::TypeDefault& stated : unit.type_defaults) {
    stated.code = BoxedCode(unit, captures, std::move(stated.code));
  }
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
