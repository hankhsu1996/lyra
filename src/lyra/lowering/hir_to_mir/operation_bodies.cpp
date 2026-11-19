#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"

#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// `value` handed to a parameter of type `taken`: borrowed where the parameter
// is a reference, mutably where it writes through it, and as its value where
// the parameter takes the value itself.
auto PassedAs(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::TypeId taken) -> mir::ExprId {
  const auto* reference = unit.types.Get(taken).As<mir::ReferenceType>();
  if (reference == nullptr) {
    return block.AsValue(unit.types, value);
  }
  switch (reference->mutability) {
    case mir::Mutability::kMutable:
      return block.Add(
          mir::Expr{.data = mir::BorrowExpr{.operand = value}, .type = taken});
    case mir::Mutability::kReadOnly:
      return mir::Borrow(unit, block, value);
  }
  throw InternalError("hir_to_mir: unknown mutability");
}

}  // namespace

auto PassedAsDeclared(
    const mir::CompilationUnit& unit, mir::Builder block,
    const mir::CallableCode& declared, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands) -> PassedOperands {
  if (receiver.has_value() != declared.receiver.has_value()) {
    throw InternalError(
        "hir_to_mir: a call of a type's operation hands a receiver where its "
        "declaration takes none, or none where it takes one -- please report "
        "this as a bug");
  }
  std::vector<mir::TypeId> taken;
  for (const mir::LocalId param : declared.params) {
    if (param != declared.receiver && param != declared.caller_location) {
      taken.push_back(declared.locals.Get(param).type);
    }
  }
  if (operands.size() != taken.size()) {
    throw InternalError(
        "hir_to_mir: a call of a type's operation hands it other operands "
        "than its declaration takes -- please report this as a bug");
  }
  for (std::size_t i = 0; i < operands.size(); ++i) {
    operands[i] = PassedAs(unit, block, operands[i], taken[i]);
  }
  return PassedOperands{
      .receiver = receiver.transform([&](mir::ExprId object) {
        return PassedAs(
            unit, block, object, declared.locals.Get(*declared.receiver).type);
      }),
      .arguments = std::move(operands)};
}

auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::DirectTarget target, support::ValueOperation answers,
    std::optional<mir::ExprId> receiver, std::vector<mir::ExprId> operands,
    mir::TypeId result) -> mir::ExprId {
  const std::optional<mir::CallableCode> declared = mir::OperationDeclarationOf(
      unit, mir::TypeOperationSource{.type = type, .answers = answers});
  if (!declared.has_value()) {
    throw InternalError(
        "hir_to_mir: a call names an operation of a type that declares none "
        "such -- please report this as a bug");
  }
  PassedOperands passed =
      PassedAsDeclared(unit, block, *declared, receiver, std::move(operands));
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = std::move(target),
                          .receiver = passed.receiver},
                  .arguments = std::move(passed.arguments)},
          .type = result});
}

}  // namespace lyra::lowering::hir_to_mir
