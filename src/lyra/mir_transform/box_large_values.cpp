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
#include "lyra/mir/code_source.hpp"
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

namespace lyra::mir_transform {

namespace {

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

// The rewrite of one body whose large values are held through a box: a local
// is declared as the box its value is built in, a parameter arrives as one,
// and every name of either reaches the value through the box. An argument of a
// large type is handed to a body this program compiles in a box built from it
// where it is handed, a capture likewise, and a large value a statement makes
// and discards is made in a box the statement discards with it. It holds what
// it learns of the body once, and has a step for each kind of node and
// statement, each handed the block it builds.
class BodyBoxing {
 public:
  BodyBoxing(const mir::CompilationUnit& unit, const mir::CallableCode& code)
      : unit_(&unit), code_(&code), local_boxes_(code.locals.size()) {
    for (const mir::LocalId id : code.locals.Ids()) {
      const mir::TypeId type = code.locals.Get(id).type;
      if (mir::IsTooLargeForAFrame(unit, type)) {
        local_boxes_.at(id.value) = mir::BoxOf(unit.types, type);
      }
    }
  }

  // The code rewritten, read where it lies.
  auto Rewritten() -> mir::CallableCode {
    return mir::CallableCode{
        .params = code_->params,
        .receiver = code_->receiver,
        .result_type = WithParamsBoxed(*unit_, code_->result_type),
        .locals = BoxedLocals(),
        .named_locals = code_->named_locals,
        .body = RewrittenBody()};
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

  auto RewrittenBody() -> std::optional<mir::Block> {
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
    return std::move(rebuild).Finish(std::move(stmts));
  }

  // `argument`, carried over to a body this program compiles: in a box built
  // from it where its type is large, which is how that body takes it. The box
  // is the callee's from the call on, so it is built where it is handed over.
  auto Argument(BlockRebuild& rebuild, mir::ExprId argument) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(argument).type;
    const mir::ExprId carried = rebuild.Expr(argument);
    if (!mir::IsTooLargeForAFrame(*unit_, type)) {
      return carried;
    }
    return BoxBuiltFrom(rebuild, mir::BoxOf(unit_->types, type), carried);
  }

  // A node carried over with what boxing changes in it. A name of a boxed
  // local reaches the value through its box, an argument of a body this
  // program compiles and a capture are taken in a box where they are large,
  // and a field of a closure holding a large value holds it in a box. Every
  // other node is left to the rebuild.
  auto Node(BlockRebuild& rebuild, const mir::Expr& expr)
      -> std::optional<mir::Expr> {
    if (const std::optional<mir::LocalId> local =
            mir::ReferencedLocal(expr.data);
        local.has_value() && local_boxes_.at(local->value).has_value()) {
      const mir::ExprId box = rebuild.Add(
          mir::MakeLocalRefExpr(*local, *local_boxes_.at(local->value)));
      return mir::MakeDerefExpr(box, expr.type);
    }
    if (const auto* call = std::get_if<mir::CallExpr>(&expr.data);
        call != nullptr && mir::EntersACompiledBody(*unit_, *call, expr.type)) {
      mir::CallExpr carried = *call;
      std::visit(
          Overloaded{
              [&](mir::Direct& callee) {
                if (callee.receiver.has_value()) {
                  callee.receiver = rebuild.Expr(*callee.receiver);
                }
              },
              [&](mir::Indirect& callee) {
                callee.code = rebuild.Expr(callee.code);
              },
              [](mir::Construct&) {},
              [&](mir::Virtual& callee) {
                callee.receiver = rebuild.Expr(callee.receiver);
              }},
          carried.callee);
      for (mir::ExprId& argument : carried.arguments) {
        argument = Argument(rebuild, argument);
      }
      return mir::Expr{.data = std::move(carried), .type = expr.type};
    }
    if (const auto* closure = std::get_if<mir::ClosureExpr>(&expr.data)) {
      mir::ClosureExpr carried = *closure;
      for (mir::FieldInit& init : carried.field_inits) {
        init.value = Argument(rebuild, init.value);
      }
      return mir::Expr{.data = std::move(carried), .type = expr.type};
    }
    if (const auto* field = std::get_if<mir::FieldAccessExpr>(&expr.data);
        field != nullptr &&
        std::holds_alternative<mir::ClosureFieldTarget>(field->field) &&
        mir::IsTooLargeForAFrame(*unit_, expr.type)) {
      const mir::ExprId held = rebuild.Add(
          mir::MakeFieldAccessExpr(
              rebuild.Expr(field->receiver), field->field,
              mir::BoxOf(unit_->types, expr.type)));
      return mir::MakeDerefExpr(held, expr.type);
    }
    return std::nullopt;
  }

  // A boxed local is declared as the box its value is built in, and a large
  // value a statement makes and discards is made in a box the statement
  // discards with it.
  auto Statement(BlockRebuild& rebuild, const mir::Stmt& stmt)
      -> std::optional<mir::StmtData> {
    if (const auto* evaluated = std::get_if<mir::ExprStmt>(&stmt.data)) {
      const mir::ExprId old = evaluated->expr;
      const mir::TypeId type = rebuild.From().exprs.Get(old).type;
      if (!mir::IsTooLargeForAFrame(*unit_, type) ||
          mir::ValueLiesInStorage(*unit_, rebuild.From(), old)) {
        return std::nullopt;
      }
      return mir::ExprStmt{
          .expr = BoxBuiltFrom(
              rebuild, mir::BoxOf(unit_->types, type), rebuild.Expr(old))};
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
};

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
  // replaced first, before any body asks how large a closure is.
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
            return cls;
          });
  return mir::RewriteEachCode(
      std::move(unit),
      [](const mir::CompilationUnit& unit, const mir::CodeSource& at,
         const mir::CallableCode& code) -> std::optional<mir::CallableCode> {
        // A foreign body is entered by the convention its declaration states,
        // which nothing here changes.
        if (mir::IsEnteredForeign(unit, at)) {
          return std::nullopt;
        }
        BodyBoxing boxing(unit, code);
        if (!boxing.ChangesAnything()) {
          return std::nullopt;
        }
        return boxing.Rewritten();
      });
}

}  // namespace lyra::mir_transform
