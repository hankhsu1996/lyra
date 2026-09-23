#include "lyra/mir/runtime_record.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::mir {

auto RuntimeRecordBuilder::FunctionRef(
    ClassId owner, const Class& cls, AbiAdapterId adapter) -> ExprId {
  const CallableCode& code = cls.abi_adapters.Get(adapter).code;
  std::vector<TypeId> params;
  params.reserve(code.params.size());
  for (const LocalId param : code.params) {
    params.push_back(code.locals.Get(param).type);
  }
  return Add(
      Expr{
          .data =
              ReferenceExpr{
                  .target =
                      lyra::mir::FunctionRef{
                          .owner = owner, .adapter = adapter}},
          .type = unit_->types.Intern(
              mir::Type{MachineFunctionType{
                  .params = std::move(params), .result = code.result_type}})});
}

auto RuntimeRecordBuilder::ErasedFunctionRef(
    ClassId owner, const Class& cls, AbiAdapterId adapter) -> ExprId {
  return Add(
      Expr{
          .data = CastExpr{.operand = FunctionRef(owner, cls, adapter)},
          .type = mir::ErasedFunction(unit_->types)});
}

auto RuntimeRecordBuilder::StringRef(const std::string& text) -> ExprId {
  const ExprId literal =
      Add(Expr{
          .data = StringLiteral{.value = text},
          .type = unit_->builtins.string});
  return Construct(
      RuntimeLibraryKind::kAbiStringRef,
      {literal, MachineInt(static_cast<std::int64_t>(text.size()))});
}

}  // namespace lyra::mir
