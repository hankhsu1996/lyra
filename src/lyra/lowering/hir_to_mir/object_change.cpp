#include "lyra/lowering/hir_to_mir/object_change.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A call to an entry acting on the object `object` reaches. The entry is handed
// the value reaching it, so where that is named where it lies -- the running
// method's own object, a handle held in a variable -- it is read.
auto Call(
    const mir::CompilationUnit& unit, mir::Builder block, support::BuiltinFn fn,
    std::optional<mir::CallPart> part, mir::ExprId object, mir::TypeId type)
    -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = fn, .part = part},
                  .arguments = {block.AsValue(unit.types, object)}},
          .type = type});
}

}  // namespace

auto ObjectEventSourceOf(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId object)
    -> mir::ExprId {
  return Call(
      unit, block, support::BuiltinFn::kObjectEventSource, std::nullopt, object,
      mir::ErasedPointer(unit.types));
}

auto PropertyStorage(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId object,
    const mir::ClassFieldTarget& property, mir::TypeId type) -> mir::Expr {
  return mir::MakeFieldAccessExpr(
      BuildObjectDeref(unit, block, object), property, type);
}

auto OpenObjectWrite(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId receiver)
    -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {block.AsValue(unit.types, receiver)}},
          .type = unit.types.Intern(
              mir::Type{mir::ObjectWriteType{
                  .object = mir::ObjectReachedThrough(
                      unit.types, block.Get(receiver).type)}})});
}

auto PropertyReference(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId receiver,
    const mir::ClassFieldTarget& property, mir::TypeId type) -> mir::ExprId {
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{mir::VariableRefType{
          .pointee = type, .mutability = mir::Mutability::kMutable}});
  return Call(
      unit, block, support::BuiltinFn::kReferProperty, property, receiver,
      reference);
}

}  // namespace lyra::lowering::hir_to_mir
