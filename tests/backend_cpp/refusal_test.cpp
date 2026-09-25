#include <gtest/gtest.h>
#include <optional>
#include <tuple>
#include <utility>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {
namespace {

auto AddBody(mir::CompilationUnit& unit, mir::CallableCode code)
    -> mir::CallableId {
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// A namespace unit whose one body evaluates a null of `type` twice, so a
// refusal that stopped at the first would be seen reporting one.
auto UnitWithNullOf(mir::TypeId (*type_of)(mir::CompilationUnit&))
    -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  mir::CallableCode empty = mir::CallableCode::Defined();
  empty.result_type = unit.builtins.void_type;
  const mir::CallableId install = AddBody(unit, empty);
  const mir::CallableId initialize = AddBody(unit, std::move(empty));
  unit.content = mir::BroughtUpNamespace{
      .install_storage = install, .initialize_storage = initialize};

  const mir::TypeId type = type_of(unit);
  mir::CallableCode code = mir::CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  for (int i = 0; i < 2; ++i) {
    const mir::ExprId null = code.Body().exprs.Add(
        mir::Expr{.data = mir::NullLiteral{}, .type = type});
    code.Body().AppendStmt(mir::ExprStmt{.expr = null});
  }
  const mir::CallableId id = AddBody(unit, std::move(code));
  unit.named_callables.push_back(mir::NamedCallable{.name = "f", .body = id});
  return unit;
}

// A node this target has no form for is reported as unsupported, every one of
// them in one emission, rather than written as text a host compiler rejects; a
// node it does have a form for reports nothing.
TEST(CppBackendRefusal, ANodeWithNoFormIsReportedAsUnsupported) {
  const mir::CompilationUnit refused =
      UnitWithNullOf([](mir::CompilationUnit& unit) {
        return unit.types.Intern(mir::Type{mir::EventType{}});
      });
  diag::DiagnosticSink refusals;
  std::ignore = EmitCppUnit(refused, refusals);
  ASSERT_EQ(refusals.Diagnostics().size(), 2U);
  for (const diag::Diagnostic& refusal : refusals.Diagnostics()) {
    EXPECT_EQ(refusal.primary.kind, diag::DiagKind::kUnsupported)
        << refusal.primary.message;
  }

  const mir::CompilationUnit written =
      UnitWithNullOf([](mir::CompilationUnit& unit) {
        return unit.types.Intern(mir::Type{mir::ChandleType{}});
      });
  diag::DiagnosticSink none;
  std::ignore = EmitCppUnit(written, none);
  EXPECT_FALSE(none.HasErrors());
}

}  // namespace
}  // namespace lyra::backend::cpp
