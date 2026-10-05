#include "lyra/lowering/hir_to_mir/object_change.hpp"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A call to an entry acting on an object, which is the first of `arguments`:
// whatever reaches the object, as the source reached it.
auto Call(
    mir::Block& block, support::BuiltinFn fn, std::optional<mir::CallPart> part,
    std::vector<mir::ExprId> arguments, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = fn, .part = part},
                  .arguments = std::move(arguments)},
          .type = type});
}

}  // namespace

auto ObjectEventSourceOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object)
    -> mir::ExprId {
  return Call(
      block, support::BuiltinFn::kObjectEventSource, std::nullopt, {object},
      mir::ErasedPointer(unit.types));
}

auto PropertyStorage(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    const PropertyName& property, mir::TypeId type) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& field) {
            return mir::MakeFieldAccessExpr(
                BuildObjectDeref(unit, block, object), field, type);
          },
          [&](const mir::CrossUnitClassFieldTarget& field) {
            return mir::MakeFieldAccessExpr(
                BuildObjectDeref(unit, block, object), field, type);
          }},
      property);
}

auto OpenObjectWrite(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {receiver}},
          .type = unit.types.Intern(
              mir::Type{mir::ObjectWriteType{
                  .object = mir::ObjectReachedThrough(
                      unit.types, block.exprs.Get(receiver).type)}})});
}

auto PropertyReference(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver,
    const PropertyName& property, mir::TypeId type) -> mir::ExprId {
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{mir::RefType{
          .pointee = type, .mutability = mir::Mutability::kMutable}});
  return std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& field) {
            return Call(
                block, support::BuiltinFn::kReferProperty, field, {receiver},
                reference);
          },
          [&](const mir::CrossUnitClassFieldTarget& field) {
            return Call(
                block, support::BuiltinFn::kReferProperty, field, {receiver},
                reference);
          }},
      property);
}

}  // namespace lyra::lowering::hir_to_mir
