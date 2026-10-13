#include "lyra/mir_transform/box_large_values.hpp"

#include <algorithm>
#include <cstdint>
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
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::mir_transform {

namespace {

// How many bytes a value of a type takes and to what it is aligned, where the
// type is laid out by what it holds: an integral, an array, a product, a union.
// Every other value is an object of the library's or a handle, a few words.
struct Layout {
  std::uint64_t size;
  std::uint64_t align;
};

auto LayoutOf(const mir::CompilationUnit& unit, mir::TypeId type) -> Layout {
  const mir::Type& ty = unit.types.Get(type);
  if (const auto* integral = ty.As<mir::IntegralType>()) {
    return Layout{
        .size = value::IntegralBytesFor(
            integral->bit_width, mir::StateDomainOf(integral->state_kind)),
        .align = value::IntegralAlignFor(integral->bit_width)};
  }
  if (const auto* array = ty.As<mir::UnpackedArrayType>()) {
    const Layout element = LayoutOf(unit, array->element_type);
    const std::uint64_t stride =
        (element.size + element.align - 1) / element.align * element.align;
    return Layout{.size = stride * array->Size(), .align = element.align};
  }
  if (const std::optional<std::span<const mir::TypeId>> components =
          mir::ProductElements(unit, type)) {
    Layout whole{.size = 0, .align = 1};
    for (const mir::TypeId component : *components) {
      const Layout part = LayoutOf(unit, component);
      whole.size =
          (whole.size + part.align - 1) / part.align * part.align + part.size;
      whole.align = std::max(whole.align, part.align);
    }
    return whole;
  }
  const std::vector<mir::TypeId>* members = nullptr;
  if (const auto* untagged = ty.As<mir::UnionType>()) {
    members = &untagged->members;
  } else if (const auto* tagged = ty.As<mir::TaggedUnionType>()) {
    members = &tagged->members;
  }
  if (members != nullptr) {
    Layout whole{.size = 0, .align = 1};
    for (const mir::TypeId member : *members) {
      const Layout part = LayoutOf(unit, member);
      whole.size = std::max(whole.size, part.size);
      whole.align = std::max(whole.align, part.align);
    }
    return whole;
  }
  return Layout{.size = sizeof(void*), .align = alignof(void*)};
}

// Whether a value of `type` is too large to be kept in a frame.
auto IsTooLargeForAFrame(const mir::CompilationUnit& unit, mir::TypeId type)
    -> bool {
  return LayoutOf(unit, type).size > value::kMaxInlineBytes;
}

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

// Whether `expr`, a node of `block`, hands an argument of a large type to a
// body this program compiles.
auto HandsOverALargeArgument(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const mir::Expr& expr) -> bool {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  return call != nullptr && EntersACompiledBody(unit, *call, expr.type) &&
         std::ranges::any_of(call->arguments, [&](mir::ExprId argument) {
           return IsTooLargeForAFrame(unit, block.exprs.Get(argument).type);
         });
}

// The same of any node of `block` or of a block nested in it.
auto AnyHandsOverALargeArgument(
    const mir::CompilationUnit& unit, const mir::Block& block) -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return HandsOverALargeArgument(unit, block, expr);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return AnyHandsOverALargeArgument(unit, child);
         });
}

// `argument`, a node of the block `rebuild` builds from, carried over, and in
// a box built from it where its type is large.
auto CarriedArgument(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild,
    mir::ExprId argument) -> mir::ExprId {
  const mir::TypeId type = rebuild.From().exprs.Get(argument).type;
  const mir::ExprId carried = rebuild.Expr(argument);
  return IsTooLargeForAFrame(unit, type)
             ? BoxBuiltFrom(rebuild, BoxOf(unit, type), carried)
             : carried;
}

// A body whose large locals and parameters are held through a box: a local is
// declared as the box its value is built in, a parameter arrives as one, every
// name of either reaches the value through the box, and an argument of a large
// type is handed to a body this program compiles in a box built from it.
// `base_args`, nodes of the body no statement reaches, are carried with it and
// are such arguments: a constructor's to its base's.
auto BoxedCode(
    const mir::CompilationUnit& unit, mir::CallableCode code,
    std::span<mir::ExprId> base_args = {}) -> mir::CallableCode {
  std::vector<std::optional<mir::TypeId>> boxed(code.locals.size());
  for (const mir::LocalId id : code.locals.Ids()) {
    const mir::TypeId type = code.locals.Get(id).type;
    if (IsTooLargeForAFrame(unit, type)) {
      boxed.at(id.value) = BoxOf(unit, type);
    }
  }
  const auto box_of = [&](mir::LocalId id) -> std::optional<mir::TypeId> {
    return boxed.at(id.value);
  };
  const bool boxes_a_local = std::ranges::any_of(
      boxed,
      [](const std::optional<mir::TypeId>& box) { return box.has_value(); });
  const bool hands_over_a_large_argument =
      code.body.has_value() &&
      (AnyHandsOverALargeArgument(unit, *code.body) ||
       std::ranges::any_of(base_args, [&](mir::ExprId argument) {
         return IsTooLargeForAFrame(unit, code.body->exprs.Get(argument).type);
       }));
  if (!boxes_a_local && !hands_over_a_large_argument) {
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
          if (!HandsOverALargeArgument(unit, rebuild.From(), expr)) {
            return std::nullopt;
          }
          mir::CallExpr carried = std::get<mir::CallExpr>(expr.data);
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
            argument = CarriedArgument(unit, rebuild, argument);
          }
          return mir::Expr{.data = std::move(carried), .type = expr.type};
        },
        .stmt = [&](BlockRebuild& rebuild,
                    const mir::Stmt& stmt) -> std::optional<mir::StmtData> {
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
  unit.classes =
      std::move(unit.classes)
          .TransformValues([&](mir::ClassId, mir::Class cls) {
            if (cls.constructor.has_value()) {
              cls.constructor->code = BoxedCode(
                  unit, std::move(cls.constructor->code),
                  cls.constructor->base_args);
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
  unit.callables =
      std::move(unit.callables)
          .TransformValues([&](mir::CallableId, mir::CallableDecl decl) {
            decl.code = BoxedCode(unit, std::move(decl.code));
            return decl;
          });
  unit.closures =
      std::move(unit.closures)
          .TransformValues([&](mir::ClosureId, mir::ClosureDecl closure) {
            closure.invoke = BoxedCode(unit, std::move(closure.invoke));
            return closure;
          });
  return unit;
}

}  // namespace lyra::mir_transform
