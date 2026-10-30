#include "lyra/lowering/hir_to_mir/pattern.hpp"

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
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Member `index` of `subject`, whose type is `subject_type`. The single step by
// which a pattern walk descends a level. It checks no tag: a pattern reaches a
// tagged union's member only behind its own test of the tag (LRM 12.6).
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
  const auto unpacked =
      [&](const std::vector<hir::UnpackedAggregateField>& fields) {
        return block.exprs.Add(StepForRead(
            owner.Unit(), block,
            MemberStep(owner, block, ty, index, member_type(fields)), subject));
      };
  if (const auto* structure = ty.As<hir::UnpackedStructType>()) {
    return unpacked(owner.Hir().DefinitionOf(*structure).fields);
  }
  if (const auto* overlay = ty.As<hir::UnpackedUnionType>()) {
    return unpacked(owner.Hir().DefinitionOf(*overlay).fields);
  }
  const PackedProjection projection = ProjectPackedAggregate(owner, ty);
  if (index.value >= projection.members.size()) {
    throw InternalError("SubjectMember: member index out of range");
  }
  return block.exprs.Add(BuildPackedMemberRead(
      owner, block, subject, projection, index,
      owner.TranslateType(projection.members[index.value].type)));
}

// The value a level of a pattern is matched against: what the construct matches
// as a whole, and the members stepped into from it. A level is read wherever
// its pattern tests it and again wherever it binds, each in a block of its
// own, so it is stated as how to reach it and read afresh at each use: `whole`
// is the local the value is held in, of `whole_type`, and a step is one member
// reached.
struct Subject {
  struct Step {
    hir::TypeId of;
    base::ComponentIndex member;
  };

  mir::LocalId whole;
  mir::TypeId whole_type;
  std::vector<Step> steps;
};

auto MemberOf(Subject subject, hir::TypeId of, base::ComponentIndex member)
    -> Subject {
  subject.steps.push_back(Subject::Step{.of = of, .member = member});
  return subject;
}

auto Read(UnitLowerer& owner, mir::Block& block, const Subject& subject)
    -> mir::ExprId {
  mir::ExprId reached = block.LocalPlace(subject.whole, subject.whole_type);
  for (const Subject::Step& step : subject.steps) {
    reached = SubjectMember(owner, block, reached, step.of, step.member);
  }
  return block.AsValue(owner.Unit().types, reached);
}

// Adds to `tests` the conditions a pattern imposes on `subject`, the value it
// is matched against, in the order they have to be tried: the pattern matches
// where every one holds, and a later one may only be evaluated once the ones
// before it held, since a tagged member is read only under its tag (LRM 11.9).
// Each is lowered where it is tried, so the tests stand beside each other
// however many members the pattern reaches. A pattern that matches
// unconditionally -- a wildcard, a bare identifier, a structure of those --
// adds nothing. How a level comes apart is read from that level's pattern
// node, which carries the type of what it matches.
template <ExprLowerer Lowerer>
void CollectPatternTests(
    Lowerer& lowerer, const Subject& subject, hir::PatternId pattern_id,
    hir::CaseKeyword keyword, std::vector<Evaluation>& tests) {
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  std::visit(
      Overloaded{
          [](const hir::WildcardPattern&) {},
          [](const hir::VariablePattern&) {},
          // The front end gives a constant the four-state form of its
          // position's type, so that it can hold the x and z bits a wildcard
          // form skips; the value is brought to that type, which loses none
          // of its bits.
          [&](const hir::ConstantPattern& c) {
            tests.emplace_back(
                [&lowerer, subject, keyword, value = c.value](
                    const WalkFrame& at) -> diag::Result<mir::ExprId> {
                  auto& owner = lowerer.Owner();
                  auto& unit = owner.Unit();
                  mir::Block& block = *at.current_block;
                  auto constant_or =
                      lowerer.LowerExpr(lowerer.HirExprs().Get(value), at);
                  if (!constant_or) {
                    return std::unexpected(std::move(constant_or.error()));
                  }
                  const mir::ExprId constant =
                      block.exprs.Add(*std::move(constant_or));
                  const mir::ExprId subject_value = Read(owner, block, subject);
                  return BuildCaseComparison(
                      unit, block, keyword,
                      AtType(
                          unit, block, subject_value,
                          block.exprs.Get(constant).type),
                      constant);
                });
          },
          [&](const hir::TaggedPattern& tagged) {
            tests.emplace_back(
                [&lowerer, subject, keyword, of = pattern.subject_type,
                 member = tagged.member_index](
                    const WalkFrame& at) -> diag::Result<mir::ExprId> {
                  auto& owner = lowerer.Owner();
                  mir::Block& block = *at.current_block;
                  return BuildHoldsMember(
                      owner, block, Read(owner, block, subject), of, member,
                      keyword);
                });
            if (!tagged.value_pattern.has_value()) return;
            CollectPatternTests(
                lowerer,
                MemberOf(subject, pattern.subject_type, tagged.member_index),
                *tagged.value_pattern, keyword, tests);
          },
          [&](const hir::StructurePattern& structure) {
            for (const auto& [field, field_pattern] :
                 structure.field_patterns) {
              CollectPatternTests(
                  lowerer,
                  MemberOf(
                      subject, pattern.subject_type,
                      base::ComponentIndex{static_cast<std::uint32_t>(field)}),
                  field_pattern, keyword, tests);
            }
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
                PatternOrigin{.pattern = pattern_id}, variable.name, type);

            mir::Block& declared = *declared_in.current_block;
            declared.AppendStmt(
                mir::LocalDeclStmt{
                    .target = local,
                    .init = declared.exprs.Add(
                        BuildDefaultValueExpr(owner.Unit(), declared, type))});
            const mir::ExprId target = assigned.LocalPlace(local, type);
            const mir::ExprId value = Read(owner, assigned, subject);
            assigned.AppendStmt(
                mir::ExprStmt{
                    .expr = assigned.exprs.Add(
                        mir::MakeAssignExpr(
                            owner.Unit().builtins, target, value))});
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
//   m = true
//   if (m) m = test 0 of the pattern
//   if (m) m = test 1 of the pattern
//   ...
//   if (m) assign the pattern's identifiers from the subject
//   m
//
// Whether it matched is read twice, to assign and as the answer, so the tests
// run once into a local.
template <ExprLowerer Lowerer>
auto PatternPredicate(
    Lowerer& lowerer, const WalkFrame& declared_in, mir::LocalId subject,
    mir::TypeId subject_type, hir::PatternId pattern, hir::CaseKeyword keyword)
    -> Predicate {
  const mir::TypeId bit = lowerer.Owner().Unit().builtins.bit1;
  return Predicate{
      .type = bit,
      .evaluate = [&lowerer, declared_in, subject, subject_type, pattern,
                   keyword](const WalkFrame& at) -> diag::Result<mir::ExprId> {
        const mir::CompilationUnit& unit = lowerer.Owner().Unit();
        BlockBuilder steps(at);
        mir::Block& body = steps.Body();
        const Subject whole{
            .whole = subject, .whole_type = subject_type, .steps = {}};

        std::vector<Evaluation> tests;
        CollectPatternTests(lowerer, whole, pattern, keyword, tests);
        const mir::TypeId boolean = unit.builtins.machine_bool;
        const mir::LocalId matched =
            steps.DeclareLocal(boolean, BuildMachineBool(unit, body, true));
        auto assigned =
            AssignAllHoldInOrder(unit, steps.Frame(), matched, tests);
        if (!assigned) return std::unexpected(std::move(assigned.error()));
        const auto read_matched = [&] {
          return body.ReadLocal(matched, boolean);
        };

        mir::Block bound;
        BindPatternIdentifiers(
            lowerer, declared_in, steps.Frame().WithBlock(&bound), whole,
            pattern);
        const mir::ExprId holds = read_matched();
        body.AppendIfThen(holds, std::move(bound));
        return at.current_block->exprs.Add(
            steps.Build(ConditionAsBit(unit, body, read_matched())));
      }};
}

template auto PatternPredicate(
    ProcessLowerer&, const WalkFrame&, mir::LocalId, mir::TypeId,
    hir::PatternId, hir::CaseKeyword) -> Predicate;
template auto PatternPredicate(
    const StructuralScopeLowerer&, const WalkFrame&, mir::LocalId, mir::TypeId,
    hir::PatternId, hir::CaseKeyword) -> Predicate;

}  // namespace lyra::lowering::hir_to_mir
