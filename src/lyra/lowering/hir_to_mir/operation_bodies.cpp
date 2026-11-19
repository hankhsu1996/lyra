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

auto CallOf(
    mir::Builder block, mir::Direct callee, std::vector<mir::ExprId> arguments,
    mir::TypeId result) -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = std::move(callee),
                  .arguments = std::move(arguments)},
          .type = result});
}

// The declaration MIR states for `answers` of `type`, where it states one.
auto DeclarationOf(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::CallableCode> {
  return mir::OperationDeclarationOf(
      unit, mir::TypeOperationSource{.type = type, .answers = answers});
}

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

auto ReceiverAsDeclared(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId object,
    support::ValueOperation answers) -> mir::ExprId {
  const std::optional<mir::CallableCode> declared =
      DeclarationOf(unit, block.Get(object).type, answers);
  if (!declared.has_value() || !declared->receiver.has_value()) {
    return mir::Borrow(unit, block, object);
  }
  return PassedAs(
      unit, block, object, declared->locals.Get(*declared->receiver).type);
}

auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::DirectTarget target, support::ValueOperation answers,
    std::optional<mir::ExprId> receiver, std::vector<mir::ExprId> operands,
    mir::TypeId result) -> mir::ExprId {
  // The operands follow the receiver among the declared parameters, where the
  // operation was called last where it reports there.
  const std::optional<mir::CallableCode> declared =
      DeclarationOf(unit, type, answers);
  std::vector<mir::TypeId> taken;
  if (declared.has_value()) {
    for (const mir::LocalId param : declared->params) {
      if (param != declared->receiver && param != declared->caller_location) {
        taken.push_back(declared->locals.Get(param).type);
      }
    }
  }
  for (std::size_t i = 0; i < operands.size(); ++i) {
    operands[i] = i < taken.size()
                      ? PassedAs(unit, block, operands[i], taken[i])
                      : mir::Borrow(unit, block, operands[i]);
  }
  return CallOf(
      block,
      mir::Direct{
          .target = std::move(target),
          .receiver = receiver.transform([&](mir::ExprId object) {
            return ReceiverAsDeclared(unit, block, object, answers);
          })},
      std::move(operands), result);
}

}  // namespace lyra::lowering::hir_to_mir
