#include "lyra/lowering/hir_to_mir/object_change.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The address of `place`, a property, as the entries naming one take it.
auto PropertyAddress(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId place)
    -> mir::ExprId {
  const mir::TypeId address_type = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = block.exprs.Get(place).type,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
  return block.exprs.Add(mir::MakeAddressOfExpr(place, address_type));
}

}  // namespace

auto ObjectRootOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver)
    -> mir::ExprId {
  const mir::Type& receiver_type =
      unit.types.Get(block.exprs.Get(receiver).type);
  if (receiver_type.Is<mir::PointerType>()) {
    return receiver;
  }
  if (!receiver_type.Is<mir::ManagedRefType>()) {
    throw InternalError(
        "ObjectRootOf: a property is reached on a class handle or on a "
        "method's receiver, and this is neither");
  }
  // The class a handle names may be one this unit cannot spell, so the object
  // is taken as the root every object shares, which the entries taking it read.
  const mir::TypeId root = unit.types.Intern(
      mir::Type{
          mir::RuntimeClassType{.which = support::RuntimeClass::kObject}});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kObjectRootOf},
                  .arguments = {receiver}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = root,
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kMutable}})});
}

auto ObjectEventSourceOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kObjectEventSource},
                  .arguments = {object}},
          .type = mir::ErasedPointer(unit.types)});
}

auto PropertyWrittenThrough(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::ExprId place) -> mir::ExprId {
  const mir::TypeId place_type = block.exprs.Get(place).type;
  const mir::ExprId address = PropertyAddress(unit, block, place);
  const mir::TypeId address_type = block.exprs.Get(address).type;
  const mir::ExprId write = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kOpenObjectWrite},
                  .arguments = {object, address}},
          .type = unit.types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kObjectWrite}})});
  // The write holds the place as the library states an address, which is read
  // again as the type the place already has.
  const mir::ExprId through = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kObjectWriteThrough,
                          .receiver = write},
                  .arguments = {}},
          .type = mir::ErasedPointer(unit.types)});
  const mir::ExprId typed = block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = through}, .type = address_type});
  return block.exprs.Add(mir::MakeDerefExpr(typed, place_type));
}

auto PropertyReferred(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::ExprId place) -> mir::ExprId {
  const mir::TypeId value = block.exprs.Get(place).type;
  const mir::ExprId storage = BuildReferenceArg(unit, block, place, value);
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kReferProperty},
                  .arguments = {object, storage}},
          .type = block.exprs.Get(storage).type});
}

}  // namespace lyra::lowering::hir_to_mir
