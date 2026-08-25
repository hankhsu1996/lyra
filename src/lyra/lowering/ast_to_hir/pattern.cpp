#include "lyra/lowering/ast_to_hir/pattern.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Patterns.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

template <ExprLowerer Lowerer>
auto LowerPattern(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Pattern& pattern,
    const slang::ast::Type& subject_type, diag::SourceSpan span)
    -> diag::Result<hir::Pattern> {
  auto subject_id_or = lowerer.Owner().InternType(subject_type, span);
  if (!subject_id_or) return std::unexpected(std::move(subject_id_or.error()));
  const hir::TypeId subject = *subject_id_or;

  switch (pattern.kind) {
    case slang::ast::PatternKind::Invalid:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "invalid pattern reached HIR lowering");

    case slang::ast::PatternKind::Wildcard:
      return hir::Pattern{
          .data = hir::WildcardPattern{},
          .subject_type = subject,
          .span = span};

    case slang::ast::PatternKind::Constant: {
      const auto& c = pattern.as<slang::ast::ConstantPattern>();
      auto value_or = lowerer.LowerExpr(c.expr, frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      const hir::ExprId value_id = frame.Exprs().Add(*std::move(value_or));
      return hir::Pattern{
          .data = hir::ConstantPattern{.value = value_id},
          .subject_type = subject,
          .span = span};
    }

    case slang::ast::PatternKind::Variable: {
      const auto& v = pattern.as<slang::ast::VariablePattern>();
      return hir::Pattern{
          .data = hir::VariablePattern{.name = std::string{v.variable.name}},
          .subject_type = subject,
          .span = span};
    }

    case slang::ast::PatternKind::Tagged: {
      const auto& t = pattern.as<slang::ast::TaggedPattern>();
      const auto& field = t.member;
      // A pattern tree names its children by id, so a nested pattern is
      // stored as it is lowered; the outermost one is stored by the entry
      // point, which is what hands its id back to the caller. The payload is
      // matched against the member's own type, which is the subject one level
      // in.
      std::optional<hir::PatternId> payload;
      if (t.valuePattern != nullptr) {
        auto inner_or =
            AddPattern(lowerer, frame, *t.valuePattern, field.getType(), span);
        if (!inner_or) return std::unexpected(std::move(inner_or.error()));
        payload = *inner_or;
      }
      return hir::Pattern{
          .data =
              hir::TaggedPattern{
                  .member_index =
                      base::ComponentIndex{
                          static_cast<std::uint32_t>(field.fieldIndex)},
                  .value_pattern = payload},
          .subject_type = subject,
          .span = span};
    }

    case slang::ast::PatternKind::Structure: {
      const auto& s = pattern.as<slang::ast::StructurePattern>();
      std::vector<std::pair<std::size_t, hir::PatternId>> fields;
      fields.reserve(s.patterns.size());
      for (const auto& fp : s.patterns) {
        auto sub_or =
            AddPattern(lowerer, frame, *fp.pattern, fp.field->getType(), span);
        if (!sub_or) return std::unexpected(std::move(sub_or.error()));
        fields.emplace_back(fp.field->fieldIndex, *sub_or);
      }
      return hir::Pattern{
          .data = hir::StructurePattern{.field_patterns = std::move(fields)},
          .subject_type = subject,
          .span = span};
    }
  }
  throw InternalError("LowerPattern: unknown slang PatternKind");
}

}  // namespace

template <ExprLowerer Lowerer>
auto AddPattern(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Pattern& pattern,
    const slang::ast::Type& subject_type, diag::SourceSpan span)
    -> diag::Result<hir::PatternId> {
  auto lowered = LowerPattern(lowerer, frame, pattern, subject_type, span);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const hir::PatternId id = frame.Patterns().Add(*std::move(lowered));
  if (pattern.kind == slang::ast::PatternKind::Variable) {
    lowerer.Owner().MapPatternVar(
        pattern.as<slang::ast::VariablePattern>().variable, id);
  }
  return id;
}

template auto AddPattern(
    ProcessLowerer&, WalkFrame, const slang::ast::Pattern&,
    const slang::ast::Type&, diag::SourceSpan) -> diag::Result<hir::PatternId>;
template auto AddPattern(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::Pattern&,
    const slang::ast::Type&, diag::SourceSpan) -> diag::Result<hir::PatternId>;

}  // namespace lyra::lowering::ast_to_hir
