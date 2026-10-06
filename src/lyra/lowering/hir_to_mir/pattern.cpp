#include "lyra/lowering/hir_to_mir/pattern.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Member `index` of `subject`, whose type is `subject_type`. The single step by
// which a pattern walk descends a level. Reaching a tagged union's member is
// checked against the tag (LRM 11.9), which the tag test below guards, so the
// walk never takes that failure.
auto SubjectMember(
    UnitLowerer& owner, mir::Block& block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index) -> mir::ExprId {
  const hir::Type& ty = owner.Hir().types.Get(subject_type);
  // Only a tagged union is destructured by a pattern (LRM 12.6): an untagged
  // one has no tag to name the component a pattern would ask for.
  const auto member_type =
      [&](const std::vector<hir::UnpackedAggregateField>& fields)
      -> mir::TypeId {
    if (index.value >= fields.size()) {
      throw InternalError("SubjectMember: member index out of range");
    }
    return owner.TranslateType(fields[index.value].type);
  };
  if (const auto* s = ty.As<hir::UnpackedStructType>()) {
    return block.exprs.Add(
        mir::MakeComponentExpr(subject, index, member_type(s->fields)));
  }
  if (const auto* u = ty.As<hir::UnpackedUnionType>()) {
    return block.exprs.Add(
        mir::MakeComponentExpr(subject, index, member_type(u->fields)));
  }
  const PackedProjection projection = ProjectPackedAggregate(owner, ty);
  if (index.value >= projection.members.size()) {
    throw InternalError("SubjectMember: member index out of range");
  }
  return block.exprs.Add(BuildPackedMemberRead(
      owner, block, subject, projection, index,
      owner.TranslateType(projection.members[index.value].type)));
}

// The test that `subject`, whose type is `subject_type`, currently holds the
// component at `index`. The two representations answer it differently -- an
// unpacked union carries the tag as its own discriminant, a packed one as bits
// of the vector -- so the test is the one place a pattern walk still sees which
// representation it is standing on.
auto BuildTagTest(
    UnitLowerer& owner, mir::Block& block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index) -> mir::ExprId {
  const hir::Type& ty = owner.Hir().types.Get(subject_type);
  if (ty.Is<hir::UnpackedUnionType>()) {
    return block.exprs.Add(
        mir::MakeTagMatchesExpr(
            subject, index, owner.Unit().builtins.machine_bool));
  }
  return BuildPackedTagTest(
      owner, block, subject, ProjectPackedAggregate(owner, ty), index);
}

// The value a level of a pattern is matched against: what the construct matches
// as a whole, and the members stepped into from it. A level is read wherever
// its pattern tests it and again wherever it binds, so it is stated as how to
// reach it and read afresh at each use: `whole` names a value and evaluates
// nothing, and a step is one member read.
struct Subject {
  struct Step {
    hir::TypeId of;
    base::ComponentIndex member;
  };

  mir::ExprId whole;
  std::vector<Step> steps;
};

auto MemberOf(Subject subject, hir::TypeId of, base::ComponentIndex member)
    -> Subject {
  subject.steps.push_back(Subject::Step{.of = of, .member = member});
  return subject;
}

auto Read(UnitLowerer& owner, mir::Block& block, const Subject& subject)
    -> mir::ExprId {
  mir::ExprId reached = subject.whole;
  for (const Subject::Step& step : subject.steps) {
    reached = SubjectMember(owner, block, reached, step.of, step.member);
  }
  return reached;
}

// Adds to `tests` the conditions a pattern imposes on `subject`, the expression
// reaching the value it is matched against, in the order they have to be tried:
// the pattern matches where every one holds, and a later one may only be
// evaluated once the ones before it held, since a tagged member is read only
// under its tag (LRM 11.9). A pattern that matches unconditionally -- a
// wildcard, a bare identifier, a structure of those -- adds nothing. Descending
// is one expression deep: how a level comes apart is read from that level's
// pattern node, which carries the type of what it matches.
template <ExprLowerer Lowerer>
auto CollectPatternTests(
    Lowerer& lowerer, WalkFrame frame, const Subject& subject,
    hir::PatternId pattern_id, std::vector<mir::ExprId>& tests)
    -> diag::Result<void> {
  auto& owner = lowerer.Owner();
  auto& unit = owner.Unit();
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  auto& block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const hir::WildcardPattern&) -> diag::Result<void> { return {}; },
          [&](const hir::VariablePattern&) -> diag::Result<void> { return {}; },
          [&](const hir::ConstantPattern& c) -> diag::Result<void> {
            auto constant_or =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.value), frame);
            if (!constant_or) {
              return std::unexpected(std::move(constant_or.error()));
            }
            const mir::ExprId constant =
                block.exprs.Add(*std::move(constant_or));
            const mir::ExprId value = Read(owner, block, subject);
            tests.push_back(block.exprs.Add(BuildMirBinaryExpr(
                unit, block, hir::BinaryOp::kEquality, value, constant,
                OneBitAnswerType(
                    unit, std::array{
                              block.exprs.Get(value).type,
                              block.exprs.Get(constant).type}))));
            return {};
          },
          [&](const hir::TaggedPattern& tagged) -> diag::Result<void> {
            tests.push_back(BuildTagTest(
                owner, block, Read(owner, block, subject), pattern.subject_type,
                tagged.member_index));
            if (!tagged.value_pattern.has_value()) return {};
            return CollectPatternTests(
                lowerer, frame,
                MemberOf(subject, pattern.subject_type, tagged.member_index),
                *tagged.value_pattern, tests);
          },
          [&](const hir::StructurePattern& structure) -> diag::Result<void> {
            for (const auto& [field, field_pattern] :
                 structure.field_patterns) {
              auto collected = CollectPatternTests(
                  lowerer, frame,
                  MemberOf(
                      subject, pattern.subject_type,
                      base::ComponentIndex{static_cast<std::uint32_t>(field)}),
                  field_pattern, tests);
              if (!collected) return collected;
            }
            return {};
          },
      },
      pattern.data);
}

// Declares each identifier the pattern introduces as a local of `declared_in`'s
// block and assigns it, from the matching position of the subject, in
// `matched`'s block; registers it so what follows resolves references to it.
template <ExprLowerer Lowerer>
void BindPatternIdentifiers(
    Lowerer& lowerer, const WalkFrame& declared_in, const WalkFrame& matched,
    const Subject& subject, hir::PatternId pattern_id) {
  auto& owner = lowerer.Owner();
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  mir::Block& assigned = *matched.current_block;
  std::visit(
      Overloaded{
          [&](const hir::WildcardPattern&) {},
          [&](const hir::ConstantPattern&) {},
          [&](const hir::VariablePattern& variable) {
            const mir::TypeId type = owner.TranslateType(pattern.subject_type);
            const mir::LocalId local = matched.bindings->DeclareNamed(
                BindingOriginId::Pattern(pattern_id), variable.name, type);

            mir::Block& declared = *declared_in.current_block;
            declared.AppendStmt(
                mir::LocalDeclStmt{
                    .target = local,
                    .init = declared.exprs.Add(
                        BuildDefaultValueExpr(owner.Unit(), declared, type))});
            assigned.AppendStmt(
                mir::ExprStmt{
                    .expr = assigned.exprs.Add(
                        mir::MakeAssignExpr(
                            owner.Unit().builtins,
                            assigned.exprs.Add(
                                mir::MakeLocalRefExpr(local, type)),
                            Read(owner, assigned, subject)))});
          },
          [&](const hir::TaggedPattern& tagged) {
            if (!tagged.value_pattern.has_value()) return;
            BindPatternIdentifiers(
                lowerer, declared_in, matched,
                MemberOf(subject, pattern.subject_type, tagged.member_index),
                *tagged.value_pattern);
          },
          [&](const hir::StructurePattern& structure) {
            for (const auto& [field, field_pattern] :
                 structure.field_patterns) {
              BindPatternIdentifiers(
                  lowerer, declared_in, matched,
                  MemberOf(
                      subject, pattern.subject_type,
                      base::ComponentIndex{static_cast<std::uint32_t>(field)}),
                  field_pattern);
            }
          },
      },
      pattern.data);
}

}  // namespace

// The match as steps:
//
//   m = every test of the pattern holds for the subject
//   if (m) assign the pattern's identifiers from the subject
//   m
//
// Whether it matched is read twice, to assign and as the answer, so the tests
// run once into a local.
template <ExprLowerer Lowerer>
auto PatternPredicate(
    Lowerer& lowerer, const WalkFrame& declared_in, mir::LocalId subject,
    mir::TypeId subject_type, hir::PatternId pattern) -> Predicate {
  const mir::TypeId bit = lowerer.Owner().Unit().builtins.bit1;
  return Predicate{
      .type = bit,
      .evaluate = [&lowerer, declared_in, subject, subject_type, pattern,
                   bit](const WalkFrame& at) -> diag::Result<mir::ExprId> {
        const mir::CompilationUnit& unit = lowerer.Owner().Unit();
        BlockBuilder steps(at);
        mir::Block& body = steps.Body();
        const auto read_subject = [&](mir::Block& block) {
          return block.exprs.Add(mir::MakeLocalRefExpr(subject, subject_type));
        };

        std::vector<mir::ExprId> tests;
        auto collected = CollectPatternTests(
            lowerer, steps.Frame(),
            Subject{.whole = read_subject(body), .steps = {}}, pattern, tests);
        if (!collected) return std::unexpected(std::move(collected.error()));
        const mir::LocalId matched = steps.DeclareLocal(
            bit, ConditionAsBit(unit, body, AllHold(unit, body, tests)));
        const auto read_matched = [&] {
          return body.exprs.Add(mir::MakeLocalRefExpr(matched, bit));
        };

        mir::Block bound;
        BindPatternIdentifiers(
            lowerer, declared_in, steps.Frame().WithBlock(&bound),
            Subject{.whole = read_subject(bound), .steps = {}}, pattern);
        body.AppendStmt(
            mir::IfStmt{
                .condition = ReduceToCondition(unit, body, read_matched()),
                .then_scope = body.child_scopes.Add(std::move(bound)),
                .else_scope = std::nullopt});
        return at.current_block->exprs.Add(steps.Build(read_matched()));
      }};
}

template auto PatternPredicate(
    ProcessLowerer&, const WalkFrame&, mir::LocalId, mir::TypeId,
    hir::PatternId) -> Predicate;
template auto PatternPredicate(
    const StructuralScopeLowerer&, const WalkFrame&, mir::LocalId, mir::TypeId,
    hir::PatternId) -> Predicate;

}  // namespace lyra::lowering::hir_to_mir
