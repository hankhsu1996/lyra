#include <array>
#include <cstddef>
#include <gtest/gtest.h>
#include <optional>
#include <tuple>
#include <utility>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {
namespace {

constexpr std::array<diag::SourceSpan, 2> kWrittenAt = {
    diag::SourceSpan{.start = 1, .end = 2},
    diag::SourceSpan{.start = 3, .end = 4}};

auto AddBody(mir::CompilationUnit& unit, mir::CallableCode code)
    -> mir::CallableId {
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
}

// A namespace unit whose one body evaluates a null of `type` twice, each
// lowered from a piece of source of its own, so a refusal that stopped at the
// first would be seen reporting one.
auto UnitWithNullOf(mir::TypeId (*type_of)(mir::CompilationUnit&))
    -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  mir::CallableCode empty = mir::CallableCode::Defined(diag::SourceSpan{});
  empty.result_type = unit.builtins.void_type;
  const mir::CallableId install = AddBody(unit, empty);
  const mir::CallableId initialize = AddBody(unit, std::move(empty));
  unit.content = mir::BroughtUpNamespace{
      .install_storage = install, .initialize_storage = initialize};

  const mir::TypeId type = type_of(unit);
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  for (const diag::SourceSpan written_at : kWrittenAt) {
    mir::Builder code_body(code.Body(), written_at);
    const mir::ExprId null =
        code_body.Add(mir::Expr{.data = mir::NullLiteral{}, .type = type});
    code_body.AppendStmt(mir::ExprStmt{.expr = null});
  }
  const mir::CallableId id = AddBody(unit, std::move(code));
  unit.named_callables.push_back(mir::NamedCallable{.name = "f", .body = id});
  return unit;
}

// A node this target has no form for is reported as unsupported, at the piece
// of source it was lowered from, every one of them in one emission, rather than
// written as text a host compiler rejects; a node it does have a form for
// reports nothing.
TEST(CppBackendRefusal, ANodeWithNoFormIsReportedAsUnsupported) {
  const mir::CompilationUnit refused =
      UnitWithNullOf([](mir::CompilationUnit& unit) {
        return unit.types.Intern(mir::Type{mir::RealType{}});
      });
  diag::DiagnosticSink refusals;
  std::ignore = EmitCppUnit(refused, refusals);
  ASSERT_EQ(refusals.Diagnostics().size(), 2U);
  for (std::size_t i = 0; i < kWrittenAt.size(); ++i) {
    const diag::Diagnostic& refusal = refusals.Diagnostics()[i];
    EXPECT_EQ(refusal.primary.kind, diag::DiagKind::kUnsupported)
        << refusal.primary.message;
    EXPECT_EQ(refusal.primary.span, diag::DiagSpan{kWrittenAt[i]})
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
