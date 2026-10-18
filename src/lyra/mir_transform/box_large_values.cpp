#include "lyra/mir_transform/box_large_values.hpp"

#include <algorithm>
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
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

// Whether a call enters a body this program compiles, whose parameters the
// same rule boxes: a subroutine, a method, a closure, the constructor of an
// object of this program's classes.
// A runtime entry and a foreign one take what the call states as it is, and so
// does an operation a type answers -- a record's method, or the body a unit
// states for a type with no declaration -- which the library reaches through
// the type's table by a convention of its own.
auto EntersACompiledBody(
    const mir::CompilationUnit& unit, const mir::CallExpr& call,
    mir::TypeId result) -> bool {
  return std::visit(
      Overloaded{
          [](const mir::Direct& direct) {
            return !std::holds_alternative<support::BuiltinFn>(direct.target) &&
                   !std::holds_alternative<mir::ForeignSymbolTarget>(
                       direct.target) &&
                   !std::holds_alternative<mir::RecordMethodTarget>(
                       direct.target) &&
                   !std::holds_alternative<mir::TypeOperationTarget>(
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

// `type` with its large values boxed: the type itself, but for the address of
// code, which takes each parameter of a large type in a box, as every body this
// program compiles does. rustc keeps `fn(T)` and decides how `T` passes by one
// query that every definition and every call asks; here the box is stated in
// MIR, so it is stated wherever a signature is.
auto WithParamsBoxed(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  if (!mir::TakesAValueTooLargeForAFrame(unit, type)) {
    return type;
  }
  mir::MachineFunctionType boxed =
      unit.types.Get(type).Get<mir::MachineFunctionType>();
  for (mir::TypeId& param : boxed.params) {
    if (mir::IsTooLargeForAFrame(unit, param)) {
      param = mir::BoxOf(unit.types, param);
    }
  }
  return unit.types.Intern(mir::Type{std::move(boxed)});
}

// The type a declared place -- a local, a capture -- holds a value of `type`
// as: in a box where the value is too large for a frame, as itself with its
// large parameters boxed otherwise.
auto HeldAs(const mir::CompilationUnit& unit, mir::TypeId type) -> mir::TypeId {
  return mir::IsTooLargeForAFrame(unit, type) ? mir::BoxOf(unit.types, type)
                                              : WithParamsBoxed(unit, type);
}

// Whether boxing changes what a value of `type` is or how it is stated.
auto BoxingChanges(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  return mir::IsTooLargeForAFrame(unit, type) ||
         mir::TakesAValueTooLargeForAFrame(unit, type);
}

// Whether boxing changes any node of `block`, or of a block nested in it.
auto BoxingChanges(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return BoxingChanges(unit, expr.type);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return BoxingChanges(unit, child);
         });
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
  BodyBoxing(const mir::CompilationUnit& unit, const mir::CallableCode& code)
      : unit_(&unit),
        code_(&code),
        local_boxes_(code.locals.size()),
        completes_(unit.types.Get(code.result_type).Is<mir::CoroutineType>()) {
    for (const mir::LocalId id : code.locals.Ids()) {
      const mir::TypeId type = code.locals.Get(id).type;
      if (mir::IsTooLargeForAFrame(unit, type)) {
        local_boxes_.at(id.value) = mir::BoxOf(unit.types, type);
      }
    }
  }

  // The code rewritten, read where it lies, with `base_args` rewritten along
  // with it: nodes of the body no statement reaches, which a constructor
  // enters its base's with, a body this program compiles.
  auto Rewritten(std::vector<mir::ExprId>& base_args) -> mir::CallableCode {
    std::optional<mir::Block> body = RewrittenBody(base_args);
    return mir::CallableCode{
        .params = code_->params,
        .receiver = code_->receiver,
        .result_type = WithParamsBoxed(*unit_, code_->result_type),
        .locals = BoxedLocals(),
        .named_locals = code_->named_locals,
        .body = std::move(body)};
  }

  // Whether the rewrite changes the body at all.
  [[nodiscard]] auto ChangesAnything() const -> bool {
    return BoxingChanges(*unit_, code_->result_type) ||
           std::ranges::any_of(
               code_->locals,
               [&](const mir::LocalDecl& local) {
                 return BoxingChanges(*unit_, local.type);
               }) ||
           (code_->body.has_value() && BoxingChanges(*unit_, *code_->body));
  }

 private:
  [[nodiscard]] auto BoxedLocals() const
      -> base::Arena<mir::LocalDecl, mir::LocalId> {
    base::Arena<mir::LocalDecl, mir::LocalId> locals;
    for (const mir::LocalId id : code_->locals.Ids()) {
      locals.Add(
          mir::LocalDecl{.type = HeldAs(*unit_, code_->locals.Get(id).type)});
    }
    return locals;
  }

  auto RewrittenBody(std::vector<mir::ExprId>& base_args)
      -> std::optional<mir::Block> {
    if (!code_->body.has_value()) {
      return std::nullopt;
    }
    const RebuildRules rules{
        .expr =
            [this](BlockRebuild& rebuild, mir::ExprId id) {
              return Node(rebuild, rebuild.From().exprs.Get(id));
            },
        .stmt = [this](
                    BlockRebuild& rebuild,
                    const mir::Stmt& stmt) { return Statement(rebuild, stmt); },
        .type =
            [this](mir::TypeId type) { return WithParamsBoxed(*unit_, type); }};
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
           !mir::ValueLiesInStorage(*unit_, rebuild.From(), operand);
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
            BoxBuiltFrom(rebuild, mir::BoxOf(unit_->types, type), carried),
            type));
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
    return BoxBuiltFrom(
        rebuild, mir::BoxOf(unit_->types, type), rebuild.Expr(argument));
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
            // The arm a conditional chooses is made where the conditional's
            // value is, as clang evaluates each arm of an aggregate conditional
            // into its destination.
            [&](const mir::ConditionalExpr& e) { return left(e); },
            [&](const mir::AssignExpr& e) {
              mir::AssignExpr carried = e;
              carried.target = rebuild.Expr(e.target);
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
            [&](const mir::UnionExpr& e) {
              mir::UnionExpr carried = e;
              carried.value = value(e.value);
              return node(std::move(carried));
            },
            // A capture is taken as an argument is: in a box built from it
            // where its value is too large for a frame.
            [&](const mir::ClosureExpr& e) {
              mir::ClosureExpr carried = e;
              for (mir::FieldInit& init : carried.field_inits) {
                init.value = Argument(rebuild, init.value);
              }
              return node(std::move(carried));
            },
            // A field of a value a node makes is a field of that value's
            // box, as rustc's `as_place` holds such a value in a temporary.
            [&](const mir::FieldAccessExpr& e) -> std::optional<mir::Expr> {
              if (!std::holds_alternative<mir::ClosureFieldTarget>(e.field) ||
                  !mir::IsTooLargeForAFrame(*unit_, expr.type)) {
                return mir::MakeFieldAccessExpr(
                    Held(rebuild, e.receiver), e.field, expr.type);
              }
              const mir::ExprId held = rebuild.Add(
                  mir::MakeFieldAccessExpr(
                      rebuild.Expr(e.receiver), e.field,
                      mir::BoxOf(unit_->types, expr.type)));
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
            // A block expression's value is made where the block's is, as
            // rustc builds a block's tail into the block's destination.
            [&](const mir::BlockExpr& e) { return left(e); },
            [&](const mir::MoveExpr& e) { return left(e); },
            [&](const mir::CopyExpr& e) { return left(e); },
            [&](const mir::DiscriminantExpr& e) { return left(e); },
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
  const mir::CallableCode* code_;
  // The box each local of a large type is held through, none for every other.
  std::vector<std::optional<mir::TypeId>> local_boxes_;
  // Whether the body is an execution, which answers through its completion.
  bool completes_;
};

// A constructor with its large values held through boxes; one with nothing to
// box is handed back as it is.
auto BoxedConstructor(
    const mir::CompilationUnit& unit, mir::ConstructorDecl constructor)
    -> mir::ConstructorDecl {
  BodyBoxing boxing(unit, constructor.code);
  if (!boxing.ChangesAnything()) {
    return constructor;
  }
  mir::CallableCode code = boxing.Rewritten(constructor.base_args);
  return mir::ConstructorDecl{
      .code = std::move(code), .base_args = std::move(constructor.base_args)};
}

// `code` with its large values held through boxes; code with nothing to box
// is handed back as it is.
auto BoxedCode(const mir::CompilationUnit& unit, mir::CallableCode code)
    -> mir::CallableCode {
  BodyBoxing boxing(unit, code);
  if (!boxing.ChangesAnything()) {
    return code;
  }
  std::vector<mir::ExprId> no_base_args;
  return boxing.Rewritten(no_base_args);
}

// `build` with its large values boxed. What a constant builds is a table of
// machine values and code addresses, so the address of code is all that
// changes in one.
auto BoxedBuild(const mir::CompilationUnit& unit, const mir::ValueBuild& build)
    -> mir::ValueBuild {
  if (!BoxingChanges(unit, build.body)) {
    return build;
  }
  const RebuildRules rules{
      .type = [&](mir::TypeId type) { return WithParamsBoxed(unit, type); }};
  BlockRebuild rebuild(build.body, rules);
  std::vector<mir::StmtId> stmts = rebuild.Stmts(build.body.root_stmts);
  const mir::ExprId value = rebuild.Expr(build.value);
  return mir::ValueBuild{
      .body = std::move(rebuild).Finish(std::move(stmts)), .value = value};
}

}  // namespace

auto BoxLargeValues(mir::CompilationUnit unit) -> mir::CompilationUnit {
  // A capture of a large value is held through its box, as a local is. A
  // closure is as large as its captures as it holds them, so the fields are
  // replaced first, before any body asks how large a closure is. Each invoke is
  // rewritten from the unit as it then stands and put in place only afterwards,
  // since rewriting reads the closures.
  std::vector<base::Arena<mir::FieldDecl, mir::FieldId>> fields(
      unit.closures.size());
  for (const mir::ClosureId id : unit.closures.Ids()) {
    const mir::ClosureDecl& closure = unit.GetClosure(id);
    for (const mir::FieldDecl& field : closure.fields) {
      fields.at(id.value).Add(mir::FieldDecl{.type = HeldAs(unit, field.type)});
    }
  }
  unit.closures =
      std::move(unit.closures)
          .TransformValues([&](mir::ClosureId id, mir::ClosureDecl closure) {
            closure.fields = std::move(fields.at(id.value));
            return closure;
          });
  unit.classes =
      std::move(unit.classes)
          .TransformValues([&](mir::ClassId, mir::Class cls) {
            base::Arena<mir::ClassConstantDecl, mir::ClassConstantId> constants;
            for (const mir::ClassConstantDecl& constant : cls.constants) {
              constants.Add(
                  mir::ClassConstantDecl{
                      .type = WithParamsBoxed(unit, constant.type),
                      .initializer = BoxedBuild(unit, constant.initializer)});
            }
            cls.constants = std::move(constants);
            cls.object_definition_initializer =
                BoxedBuild(unit, cls.object_definition_initializer);
            if (cls.constructor.has_value()) {
              cls.constructor =
                  BoxedConstructor(unit, *std::move(cls.constructor));
            }
            cls.callables =
                std::move(cls.callables)
                    .TransformValues(
                        [&](mir::CallableId, mir::CallableDecl decl) {
                          decl.code = BoxedCode(unit, std::move(decl.code));
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
              decl.code = BoxedCode(unit, std::move(decl.code));
            }
            return decl;
          });
  // The body a foreign name's scope entries share is this unit's own. What it
  // takes the foreign side passes, by value only where the value is small (LRM
  // H.8.1), and it calls the subroutine of the scope it resolves as any call
  // does.
  for (mir::ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    entry.definition = BoxedCode(unit, std::move(entry.definition));
  }
  unit.record_methods =
      std::move(unit.record_methods)
          .TransformValues([&](mir::RecordId, mir::RecordMethods methods) {
            for (mir::RecordMethod& method : methods) {
              method.code = BoxedCode(unit, std::move(method.code));
            }
            return methods;
          });
  for (mir::TypeOperation& stated : unit.type_operations) {
    stated.code = BoxedCode(unit, std::move(stated.code));
  }
  std::vector<mir::CallableCode> invokes;
  invokes.reserve(unit.closures.size());
  for (const mir::ClosureId id : unit.closures.Ids()) {
    invokes.push_back(BoxedCode(unit, unit.GetClosure(id).invoke));
  }
  unit.closures =
      std::move(unit.closures)
          .TransformValues([&](mir::ClosureId id, mir::ClosureDecl closure) {
            closure.invoke = std::move(invokes.at(id.value));
            return closure;
          });
  return unit;
}

}  // namespace lyra::mir_transform
