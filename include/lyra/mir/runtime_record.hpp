#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Builds runtime-library records into an expression arena. Each record is an
// ordinary constructed value whose type names the runtime type it builds, so a
// record is stated in MIR rather than assembled by each backend from the parts
// it describes.
class RuntimeRecordBuilder {
 public:
  RuntimeRecordBuilder(
      const CompilationUnit& unit, base::Arena<Expr, ExprId>& exprs)
      : unit_(&unit), exprs_(&exprs) {
  }

  [[nodiscard]] auto Type(RuntimeLibraryKind kind) const -> TypeId {
    return unit_->types.Intern(mir::Type{RuntimeLibraryType{.kind = kind}});
  }

  auto Add(Expr expr) -> ExprId {
    return exprs_->Add(std::move(expr));
  }

  [[nodiscard]] auto TypeOf(ExprId expr) const -> TypeId {
    return exprs_->Get(expr).type;
  }

  auto Construct(RuntimeLibraryKind kind, std::vector<ExprId> args) -> ExprId {
    return Add(
        Expr{
            .data =
                CallExpr{
                    .callee = lyra::mir::Construct{},
                    .arguments = std::move(args)},
            .type = Type(kind)});
  }

  auto Bool(bool value) -> ExprId {
    return Add(
        Expr{
            .data = MachineBoolLiteral{.value = value},
            .type = unit_->builtins.machine_bool});
  }

  auto MachineInt(std::int64_t value) -> ExprId {
    return Add(
        Expr{
            .data = MachineIntLiteral{.value = value},
            .type = unit_->builtins.machine_int64});
  }

  // A contiguous run of already-built records, as the aggregate a runtime
  // record reads one of. The element type is a parameter rather than read off
  // the elements, so a run with none still has one.
  auto MachineArray(TypeId element, std::vector<ExprId> elements) -> ExprId {
    const auto size = static_cast<std::uint32_t>(elements.size());
    return Add(
        Expr{
            .data = ArrayLiteralExpr{.elements = std::move(elements)},
            .type = MachineArrayOf(unit_->types, element, size)});
  }

  // The address of `adapter`, typed as the function it is. A backend that must
  // name that type -- to erase it, or to restore it -- reads it off the node
  // rather than off a convention the two sides would have to keep in step.
  auto FunctionRef(const Class& cls, AbiAdapterId adapter) -> ExprId;

  // The adapter's address named as the erased entry type, so entries of
  // different prototypes share one table. It is restored to the prototype it
  // was generated with at the one place that calls it.
  auto ErasedFunctionRef(const Class& cls, AbiAdapterId adapter) -> ExprId;

  auto StringRef(const std::string& text) -> ExprId;

 private:
  const CompilationUnit* unit_;
  base::Arena<Expr, ExprId>* exprs_;
};

}  // namespace lyra::mir
