#include "lyra/lowering/hir_to_mir/pattern_rendering.hpp"

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/format.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// How much white space the pattern carries is the tool's to choose -- LRM
// 21.2.1.6 asks only that the result read as the assignment-pattern syntax --
// so the choice is made here and every type spells it the same way.
constexpr std::string_view kOpen = "'{";
constexpr std::string_view kClose = "}";
constexpr std::string_view kSeparator = ", ";
constexpr std::string_view kNameMark = ":";

// An element of a pattern occupies no field of its own (LRM 21.2.1.6), which
// is what the spec a leaf is handed asks for.
constexpr std::int32_t kNoField = 0;

// The index an associative traversal reports it visited: a completion carries
// the result first and then what each output formal received (LRM 7.9.4), and
// this entry has one.
constexpr base::ComponentIndex kVisitedIndex{1};

// Which rule of LRM 21.2.1.6 a type reads under, at the grain the rendering is
// built by. `kValueDecides` is every type the clause sends to the value's own
// unformatted rendering, which the runtime formatter answers; the rest each
// name a fact only a declaration carries. What leaves this file is the coarser
// question a caller assembling an operand asks, derived from this one so the
// two cannot disagree.
enum class RenderingKind : std::uint8_t {
  kValueDecides,
  kEnumeration,
  kUnpackedAggregate,
  kPackedAggregate,
  kIndexedElements,
  kAssociativeEntries,
};

auto Read(const WalkFrame& frame, mir::LocalId local, mir::TypeId type)
    -> mir::ExprId {
  return frame.current_block->exprs.Add(mir::MakeLocalRefExpr(local, type));
}

auto Assign(
    const WalkFrame& frame, mir::LocalId target, mir::TypeId type,
    mir::ExprId value) -> mir::ExprId {
  return frame.current_block->exprs.Add(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target = Read(frame, target, type), .value = value},
          .type = type});
}

auto RenderingKindOf(const UnitLowerer& unit_lowerer, hir::TypeId type)
    -> RenderingKind {
  const hir::Type& t = unit_lowerer.Hir().types.Get(type);
  if (t.Is<hir::EnumType>()) return RenderingKind::kEnumeration;
  if (t.Is<hir::UnpackedStructType>() || t.Is<hir::UnpackedUnionType>()) {
    return RenderingKind::kUnpackedAggregate;
  }
  if (t.Is<hir::PackedStructType>() || t.Is<hir::PackedUnionType>()) {
    return RenderingKind::kPackedAggregate;
  }
  if (t.Is<hir::AssociativeArrayType>()) {
    return RenderingKind::kAssociativeEntries;
  }
  if (t.Is<hir::UnpackedArrayType>() || t.Is<hir::DynamicArrayType>() ||
      t.Is<hir::QueueType>()) {
    return RenderingKind::kIndexedElements;
  }
  return RenderingKind::kValueDecides;
}

// Builds one type's rendering and every rendering it reaches. Each homes on the
// class the print site is being lowered into, which is what the per-unit cache
// keys it beside.
class Renderer {
 public:
  Renderer(UnitLowerer& unit_lowerer, WalkFrame home, diag::SourceSpan span)
      : unit_lowerer_(&unit_lowerer), home_(home), span_(span) {
  }

  // The text a value of `type` reads as, written into the block `frame` names.
  auto Render(const WalkFrame& frame, mir::ExprId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;

 private:
  auto Owner() -> UnitLowerer& {
    return *unit_lowerer_;
  }
  auto Unit() -> mir::CompilationUnit& {
    return unit_lowerer_->Unit();
  }
  auto StringType() -> mir::TypeId {
    return Unit().builtins.string;
  }
  auto IntType() -> mir::TypeId {
    return Unit().builtins.int_type;
  }
  auto HirType(hir::TypeId id) -> const hir::Type& {
    return unit_lowerer_->Hir().types.Get(id);
  }

  auto Text(const WalkFrame& frame, std::string_view literal) -> mir::ExprId;
  auto Join(const WalkFrame& frame, std::vector<mir::ExprId> parts)
      -> mir::ExprId;
  auto FormatLeaf(const WalkFrame& frame, mir::ExprId value, mir::TypeId type)
      -> mir::ExprId;
  auto Named(
      const WalkFrame& frame, const std::string& name, mir::ExprId element_text)
      -> mir::ExprId;

  auto Helper(hir::TypeId type) -> diag::Result<mir::CallableTarget>;
  auto Synthesize(hir::TypeId type) -> diag::Result<mir::CallableId>;
  auto BuildBody(const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;

  auto UnpackedMember(
      const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
      const hir::UnpackedAggregateField& field, base::ComponentIndex index)
      -> diag::Result<mir::ExprId>;
  auto PackedMember(
      const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
      const PackedProjection& projection,
      const hir::PackedAggregateField& field, base::ComponentIndex index)
      -> diag::Result<mir::ExprId>;

  auto BuildUnpackedAggregate(
      const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;
  auto BuildPackedAggregate(
      const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;
  auto BuildEnumeration(
      const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;
  auto BuildIndexedElements(
      const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;
  auto BuildAssociativeEntries(
      const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
      -> diag::Result<mir::ExprId>;
  auto TraversalStep(
      const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
      mir::LocalId key, mir::TypeId key_type, mir::TypeId visit_type,
      support::BuiltinFn entry) -> mir::ExprId;

  UnitLowerer* unit_lowerer_;
  WalkFrame home_;
  diag::SourceSpan span_;
};

auto Renderer::Text(const WalkFrame& frame, std::string_view literal)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const mir::ExprId raw = block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::string{literal}},
          .type = StringType()});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {raw}},
          .type = StringType()});
}

// LRM 11.4.12: what a concatenation joins follows the operand's value domain,
// so joining strings is the entry that joins bit planes, folded into a chain
// because one entry takes two operands.
auto Renderer::Join(const WalkFrame& frame, std::vector<mir::ExprId> parts)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  mir::ExprId joined = parts.front();
  for (std::size_t i = 1; i < parts.size(); ++i) {
    joined = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kConcat,
                            .receiver = joined},
                    .arguments = {parts[i]}},
            .type = StringType()});
  }
  return joined;
}

auto Renderer::FormatLeaf(
    const WalkFrame& frame, mir::ExprId value, mir::TypeId type)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const std::vector<mir::RuntimePrintItem> items = {mir::RuntimePrintValue(
      value, type,
      mir::FormatSpec(
          value::FormatKind::kAssignmentPattern,
          mir::FormatModifiers{.width = kNoField}))};
  const mir::ExprId array =
      block.exprs.Add(BuildPrintItemsArray(Unit(), block, items, 0));
  const mir::ExprId runtime =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(Owner()));
  return block.exprs.Add(BuildFormatCallExpr(Unit(), block, runtime, array));
}

auto Renderer::Named(
    const WalkFrame& frame, const std::string& name, mir::ExprId element_text)
    -> mir::ExprId {
  const mir::ExprId label = Text(frame, name + std::string{kNameMark});
  return Join(frame, {label, element_text});
}

auto Renderer::Render(
    const WalkFrame& frame, mir::ExprId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  if (RenderingKindOf(Owner(), type) == RenderingKind::kValueDecides) {
    return FormatLeaf(frame, value, Owner().TranslateType(type));
  }
  auto target_or = Helper(type);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  return frame.current_block->exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = *target_or},
                  .arguments = {value}},
          .type = StringType()});
}

auto Renderer::Helper(hir::TypeId type) -> diag::Result<mir::CallableTarget> {
  auto& cache = Owner().PatternRenderHelpers();
  if (const auto it = cache.find(type); it != cache.end()) {
    return it->second;
  }
  auto slot_or = Synthesize(type);
  if (!slot_or) return std::unexpected(std::move(slot_or.error()));
  const mir::CallableTarget target{
      .owner = home_.current_class_id, .slot = *slot_or};
  cache.emplace(type, target);
  return target;
}

auto Renderer::Synthesize(hir::TypeId type) -> diag::Result<mir::CallableId> {
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(Unit(), code);
  const mir::LocalId value = code.AddLocal(Owner().TranslateType(type));
  code.params = {value};
  code.result_type = StringType();

  WalkFrame frame = home_;
  frame.bindings = &bindings;
  frame.current_block = &code.Body();

  auto text_or = BuildBody(frame, value, type);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  code.Body().AppendStmt(mir::ReturnStmt{.value = *text_or});

  return home_.current_class->callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

auto Renderer::BuildBody(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  switch (RenderingKindOf(Owner(), type)) {
    case RenderingKind::kEnumeration:
      return BuildEnumeration(frame, value, type);
    case RenderingKind::kUnpackedAggregate:
      return BuildUnpackedAggregate(frame, value, type);
    case RenderingKind::kPackedAggregate:
      return BuildPackedAggregate(frame, value, type);
    case RenderingKind::kIndexedElements:
      return BuildIndexedElements(frame, value, type);
    case RenderingKind::kAssociativeEntries:
      return BuildAssociativeEntries(frame, value, type);
    case RenderingKind::kValueDecides:
      throw InternalError(
          "Renderer::BuildBody: a type whose value decides its own rendering "
          "has no rendering to synthesize");
  }
  throw InternalError("Renderer::BuildBody: unknown rendering kind");
}

auto Renderer::UnpackedMember(
    const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
    const hir::UnpackedAggregateField& field, base::ComponentIndex index)
    -> diag::Result<mir::ExprId> {
  // LRM 7.3.2 allows a tagged union member declared `void`, which is all
  // information in the tag: the name is the whole of what there is to print.
  if (HirType(field.type).Is<hir::VoidType>()) {
    return Text(frame, field.name);
  }
  mir::Block& block = *frame.current_block;
  const mir::ExprId subject = Read(frame, value, mir_type);
  const mir::ExprId member = block.exprs.Add(
      mir::MakePartAccessExpr(
          subject, index, Owner().TranslateType(field.type)));
  auto text_or = Render(frame, member, field.type);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  return Named(frame, field.name, *text_or);
}

// Reads the run the member occupies rather than the member the source named:
// the rendering has already settled which member it is printing -- every one
// of a structure's, the first of a union's, the one a tag names -- so the
// check LRM 11.9 puts on a written access would be this operation asking again
// what it just decided.
auto Renderer::PackedMember(
    const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
    const PackedProjection& projection, const hir::PackedAggregateField& field,
    base::ComponentIndex index) -> diag::Result<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  const ProjectedMember& run = projection.members.at(index.value);
  const mir::ExprId subject = Read(frame, value, mir_type);
  const mir::ExprId member = block.exprs.Add(BuildPackedRunRead(
      Owner(), block, subject, run.bit_offset, run.bit_width,
      Owner().TranslateType(field.type)));
  auto text_or = Render(frame, member, field.type);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  return Named(frame, field.name, *text_or);
}

auto Renderer::BuildUnpackedAggregate(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const hir::Type& t = HirType(type);
  const bool is_union = t.Is<hir::UnpackedUnionType>();
  const bool tagged = is_union && t.Get<hir::UnpackedUnionType>().tagged;
  const std::vector<hir::UnpackedAggregateField>& fields =
      is_union ? t.Get<hir::UnpackedUnionType>().fields
               : t.Get<hir::UnpackedStructType>().fields;
  const mir::TypeId mir_type = Owner().TranslateType(type);
  mir::Block& block = *frame.current_block;

  std::vector<mir::ExprId> parts;
  parts.push_back(Text(frame, kOpen));
  if (tagged) {
    // LRM 21.2.1.6 prints the member the tag names, so each position is asked
    // whether it is the live one -- which is also what keeps a member the tag
    // does not select from being read at all (LRM 11.9).
    mir::ExprId chain = Text(frame, "");
    for (std::size_t i = fields.size(); i-- > 0;) {
      const auto index = base::ComponentIndex{static_cast<std::uint32_t>(i)};
      const mir::ExprId subject = Read(frame, value, mir_type);
      const mir::ExprId live = block.exprs.Add(
          mir::MakeTagMatchesExpr(
              subject, index, Unit().builtins.machine_bool));
      auto member_or = UnpackedMember(frame, value, mir_type, fields[i], index);
      if (!member_or) return std::unexpected(std::move(member_or.error()));
      chain = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition = ReduceToCondition(Unit(), block, live),
                      .then_value = *member_or,
                      .else_value = chain},
              .type = StringType()});
    }
    parts.push_back(chain);
  } else {
    // A union prints only its first declared member (LRM 21.2.1.6); a
    // structure prints every one of them.
    const std::size_t count = is_union ? std::size_t{1} : fields.size();
    for (std::size_t i = 0; i < count; ++i) {
      if (i != 0) parts.push_back(Text(frame, kSeparator));
      const auto index = base::ComponentIndex{static_cast<std::uint32_t>(i)};
      auto member_or = UnpackedMember(frame, value, mir_type, fields[i], index);
      if (!member_or) return std::unexpected(std::move(member_or.error()));
      parts.push_back(*member_or);
    }
  }
  parts.push_back(Text(frame, kClose));
  return Join(frame, std::move(parts));
}

auto Renderer::BuildPackedAggregate(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const hir::Type& t = HirType(type);
  const bool is_union = t.Is<hir::PackedUnionType>();
  const bool tagged = is_union && t.Get<hir::PackedUnionType>().tagged;
  const std::vector<hir::PackedAggregateField>& fields =
      is_union ? t.Get<hir::PackedUnionType>().fields
               : t.Get<hir::PackedStructType>().fields;
  const mir::TypeId mir_type = Owner().TranslateType(type);
  const PackedProjection projection = ProjectPackedAggregate(Owner(), t);
  mir::Block& block = *frame.current_block;

  std::vector<mir::ExprId> parts;
  parts.push_back(Text(frame, kOpen));
  if (tagged) {
    mir::ExprId chain = Text(frame, "");
    for (std::size_t i = fields.size(); i-- > 0;) {
      const auto index = base::ComponentIndex{static_cast<std::uint32_t>(i)};
      const mir::ExprId subject = Read(frame, value, mir_type);
      const mir::ExprId live =
          BuildPackedTagTest(Owner(), block, subject, projection, index);
      auto member_or =
          PackedMember(frame, value, mir_type, projection, fields[i], index);
      if (!member_or) return std::unexpected(std::move(member_or.error()));
      chain = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition = ReduceToCondition(Unit(), block, live),
                      .then_value = *member_or,
                      .else_value = chain},
              .type = StringType()});
    }
    parts.push_back(chain);
  } else {
    const std::size_t count = is_union ? std::size_t{1} : fields.size();
    for (std::size_t i = 0; i < count; ++i) {
      if (i != 0) parts.push_back(Text(frame, kSeparator));
      const auto index = base::ComponentIndex{static_cast<std::uint32_t>(i)};
      auto member_or =
          PackedMember(frame, value, mir_type, projection, fields[i], index);
      if (!member_or) return std::unexpected(std::move(member_or.error()));
      parts.push_back(*member_or);
    }
  }
  parts.push_back(Text(frame, kClose));
  return Join(frame, std::move(parts));
}

// LRM 21.2.1.6: an enumeration prints the name its type declares for the
// value, and the base type's own rendering for a value the type declares no
// name for. `name` answers with the empty string in exactly that case (LRM
// 6.19.5), so the length of its answer is what chooses between the two. The
// text prints unquoted, a quoted element being what the clause asks of a
// string rather than of an enumeration.
auto Renderer::BuildEnumeration(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const mir::TypeId mir_type = Owner().TranslateType(type);
  mir::Block& block = *frame.current_block;

  auto name_or = BuildEnumNameCallExpr(
      Owner(), frame, Read(frame, value, mir_type), mir_type, span_);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  const mir::ExprId name = block.exprs.Add(*std::move(name_or));
  const mir::ExprId base_text =
      FormatLeaf(frame, Read(frame, value, mir_type), mir_type);

  const mir::ExprId length = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kLen, .receiver = name},
                  .arguments = {}},
          .type = IntType()});
  const mir::ExprId has_name = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kInequality,
                  .lhs = length,
                  .rhs = BuildIntLiteral(Unit(), block, 0)},
          .type = Unit().builtins.bit1});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = ReduceToCondition(Unit(), block, has_name),
                  .then_value = name,
                  .else_value = base_text},
          .type = StringType()});
}

// The elements of a container reached by position (LRM 7.4 / 7.5 / 7.10),
// joined in index order. The index is the coordinate the source would have
// written, so a declared range that neither starts at zero nor ascends takes
// its own -- the ordinal beside it is what the separator and the bound read.
auto Renderer::BuildIndexedElements(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const hir::Type& t = HirType(type);
  const hir::TypeId element_type = *t.ContainerElementType();
  const mir::TypeId mir_type = Owner().TranslateType(type);
  const mir::TypeId element_mir = Owner().TranslateType(element_type);
  mir::Block& block = *frame.current_block;

  std::int64_t first_index = 0;
  std::int64_t index_step = 1;
  mir::ExprId count{};
  if (const auto* fixed = t.As<hir::UnpackedArrayType>()) {
    first_index = fixed->dim.left;
    index_step = fixed->dim.left <= fixed->dim.right ? 1 : -1;
    count = BuildIntLiteral(
        Unit(), block, static_cast<std::int64_t>(fixed->dim.ElementCount()));
  } else {
    count = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kSize,
                            .receiver = Read(frame, value, mir_type)},
                    .arguments = {}},
            .type = IntType()});
  }

  const mir::LocalId text = frame.bindings->DeclareAnonymous(StringType());
  block.AppendStmt(
      mir::LocalDeclStmt{.target = text, .init = Text(frame, kOpen)});
  // The index runs in the container's own coordinates and the ordinal counts
  // from zero, so they are two locals; only one of them can be the loop's own
  // declaration, which is why the index is declared ahead of it.
  const mir::LocalId index = frame.bindings->DeclareAnonymous(IntType());
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = index,
          .init = BuildIntLiteral(Unit(), block, first_index)});
  const mir::LocalId ordinal = frame.bindings->DeclareAnonymous(IntType());

  mir::Block loop_body;
  const WalkFrame body_frame = frame.WithBlock(&loop_body);
  {
    const mir::ExprId separator =
        loop_body.exprs.Add(
            mir::Expr{
                .data =
                    mir::ConditionalExpr{
                        .condition = ReduceToCondition(
                            Unit(), loop_body,
                            loop_body.exprs.Add(
                                mir::Expr{
                                    .data =
                                        mir::BinaryExpr{
                                            .op = mir::BinaryOp::kInequality,
                                            .lhs = Read(
                                                body_frame, ordinal, IntType()),
                                            .rhs = BuildIntLiteral(
                                                Unit(), loop_body, 0)},
                                    .type = Unit().builtins.bit1})),
                        .then_value = Text(body_frame, kSeparator),
                        .else_value = Text(body_frame, "")},
                .type = StringType()});
    const mir::ExprId element = loop_body.exprs.Add(BuildElementAccessCallExpr(
        Owner(), loop_body, Read(body_frame, value, mir_type),
        Read(body_frame, index, IntType()), element_mir));
    auto element_text_or = Render(body_frame, element, element_type);
    if (!element_text_or) {
      return std::unexpected(std::move(element_text_or.error()));
    }
    const mir::ExprId grown = Join(
        body_frame,
        {Read(body_frame, text, StringType()), separator, *element_text_or});
    loop_body.AppendStmt(
        mir::ExprStmt{.expr = Assign(body_frame, text, StringType(), grown)});
  }
  const mir::BlockId loop_scope = block.child_scopes.Add(std::move(loop_body));

  const mir::ExprId bound = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessThan,
                  .lhs = Read(frame, ordinal, IntType()),
                  .rhs = count},
          .type = Unit().builtins.bit1});
  const mir::ExprId next_ordinal = Assign(
      frame, ordinal, IntType(),
      block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kAdd,
                      .lhs = Read(frame, ordinal, IntType()),
                      .rhs = BuildIntLiteral(Unit(), block, 1)},
              .type = IntType()}));
  const mir::ExprId next_index = Assign(
      frame, index, IntType(),
      block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kAdd,
                      .lhs = Read(frame, index, IntType()),
                      .rhs = BuildIntLiteral(Unit(), block, index_step)},
              .type = IntType()}));

  std::vector<mir::ForInit> init;
  init.emplace_back(
      mir::ForInitDecl{
          .induction_var = ordinal, .init = BuildIntLiteral(Unit(), block, 0)});
  block.AppendStmt(
      mir::ForStmt{
          .init = std::move(init),
          .condition = ReduceToCondition(Unit(), block, bound),
          .step = {next_ordinal, next_index},
          .scope = loop_scope});

  return Join(frame, {Read(frame, text, StringType()), Text(frame, kClose)});
}

// One step of the LRM 7.9.4 / 7.9.6 traversal: the call reports whether it
// visited an entry and hands back the index it visited, which the step writes
// into the key before the body reads it.
auto Renderer::TraversalStep(
    const WalkFrame& frame, mir::LocalId value, mir::TypeId mir_type,
    mir::LocalId key, mir::TypeId key_type, mir::TypeId visit_type,
    support::BuiltinFn entry) -> mir::ExprId {
  BlockBuilder step(frame);
  const WalkFrame& inner = step.Frame();
  mir::Block& body = step.Body();

  const mir::ExprId visited = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = entry,
                          .receiver = Read(inner, value, mir_type)},
                  .arguments = {Read(inner, key, key_type)}},
          .type = visit_type});
  const mir::LocalId visit = step.Bindings().DeclareAnonymous(visit_type);
  body.AppendStmt(mir::LocalDeclStmt{.target = visit, .init = visited});

  const mir::ExprId index = body.exprs.Add(
      mir::MakePartAccessExpr(
          Read(inner, visit, visit_type), kVisitedIndex, key_type));
  body.AppendStmt(mir::ExprStmt{.expr = Assign(inner, key, key_type, index)});

  const mir::ExprId more = body.exprs.Add(
      mir::MakePartAccessExpr(
          Read(inner, visit, visit_type), kCompletionResult, IntType()));
  return frame.current_block->exprs.Add(step.Build(more));
}

// The entries of an associative array (LRM 7.8), each printed under the index
// it is stored at -- which is what the clause's own example shows, and what a
// container whose positions are otherwise unknowable needs.
auto Renderer::BuildAssociativeEntries(
    const WalkFrame& frame, mir::LocalId value, hir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const auto& array = HirType(type).Get<hir::AssociativeArrayType>();
  const mir::TypeId mir_type = Owner().TranslateType(type);
  const mir::TypeId key_type = Owner().TranslateType(array.key_type);
  const mir::TypeId element_mir = Owner().TranslateType(array.element_type);
  const mir::TypeId visit_type =
      CompletionPayloadType(Unit(), {IntType(), key_type});
  mir::Block& block = *frame.current_block;

  const mir::LocalId text = frame.bindings->DeclareAnonymous(StringType());
  block.AppendStmt(
      mir::LocalDeclStmt{.target = text, .init = Text(frame, kOpen)});
  const mir::LocalId key = frame.bindings->DeclareAnonymous(key_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = key,
          .init =
              block.exprs.Add(BuildDefaultValueExpr(Unit(), block, key_type))});
  const mir::LocalId ordinal = frame.bindings->DeclareAnonymous(IntType());
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = ordinal, .init = BuildIntLiteral(Unit(), block, 0)});
  const mir::LocalId more = frame.bindings->DeclareAnonymous(IntType());

  mir::Block loop_body;
  const WalkFrame body_frame = frame.WithBlock(&loop_body);
  {
    const mir::ExprId separator =
        loop_body.exprs.Add(
            mir::Expr{
                .data =
                    mir::ConditionalExpr{
                        .condition = ReduceToCondition(
                            Unit(), loop_body,
                            loop_body.exprs.Add(
                                mir::Expr{
                                    .data =
                                        mir::BinaryExpr{
                                            .op = mir::BinaryOp::kInequality,
                                            .lhs = Read(
                                                body_frame, ordinal, IntType()),
                                            .rhs = BuildIntLiteral(
                                                Unit(), loop_body, 0)},
                                    .type = Unit().builtins.bit1})),
                        .then_value = Text(body_frame, kSeparator),
                        .else_value = Text(body_frame, "")},
                .type = StringType()});
    auto index_text_or =
        Render(body_frame, Read(body_frame, key, key_type), array.key_type);
    if (!index_text_or) {
      return std::unexpected(std::move(index_text_or.error()));
    }
    const mir::ExprId element = loop_body.exprs.Add(BuildElementAccessCallExpr(
        Owner(), loop_body, Read(body_frame, value, mir_type),
        Read(body_frame, key, key_type), element_mir));
    auto element_text_or = Render(body_frame, element, array.element_type);
    if (!element_text_or) {
      return std::unexpected(std::move(element_text_or.error()));
    }
    const mir::ExprId grown = Join(
        body_frame,
        {Read(body_frame, text, StringType()), separator, *index_text_or,
         Text(body_frame, kNameMark), *element_text_or});
    loop_body.AppendStmt(
        mir::ExprStmt{.expr = Assign(body_frame, text, StringType(), grown)});
  }
  const mir::BlockId loop_scope = block.child_scopes.Add(std::move(loop_body));

  const mir::ExprId first = TraversalStep(
      frame, value, mir_type, key, key_type, visit_type,
      support::BuiltinFn::kAssocFirst);
  const mir::ExprId next = TraversalStep(
      frame, value, mir_type, key, key_type, visit_type,
      support::BuiltinFn::kAssocNext);
  const mir::ExprId next_more = Assign(frame, more, IntType(), next);
  const mir::ExprId next_ordinal = Assign(
      frame, ordinal, IntType(),
      block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kAdd,
                      .lhs = Read(frame, ordinal, IntType()),
                      .rhs = BuildIntLiteral(Unit(), block, 1)},
              .type = IntType()}));

  std::vector<mir::ForInit> init;
  init.emplace_back(mir::ForInitDecl{.induction_var = more, .init = first});
  block.AppendStmt(
      mir::ForStmt{
          .init = std::move(init),
          .condition =
              ReduceToCondition(Unit(), block, Read(frame, more, IntType())),
          .step = {next_more, next_ordinal},
          .scope = loop_scope});

  return Join(frame, {Read(frame, text, StringType()), Text(frame, kClose)});
}

}  // namespace

auto BuildPatternRendering(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId value,
    hir::TypeId type, diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  // The callables home on a class an intra-unit call can name; a package
  // namespace has none.
  if (frame.current_class == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "the assignment-pattern format of a declared type in a package "
        "context is not yet supported");
  }
  Renderer renderer(unit_lowerer, frame, span);
  return renderer.Render(frame, value, type);
}

auto PatternRenderingOf(const UnitLowerer& unit_lowerer, hir::TypeId type)
    -> PatternRendering {
  switch (RenderingKindOf(unit_lowerer, type)) {
    case RenderingKind::kValueDecides:
      return PatternRendering::kValueDecides;
    case RenderingKind::kEnumeration:
      return PatternRendering::kBesideTheValue;
    case RenderingKind::kUnpackedAggregate:
    case RenderingKind::kPackedAggregate:
    case RenderingKind::kIndexedElements:
    case RenderingKind::kAssociativeEntries:
      return PatternRendering::kInsteadOfTheValue;
  }
  throw InternalError("PatternRenderingOf: unknown rendering kind");
}

auto TypeStatesItsRendering(PatternRendering rendering) -> bool {
  switch (rendering) {
    case PatternRendering::kValueDecides:
      return false;
    case PatternRendering::kBesideTheValue:
    case PatternRendering::kInsteadOfTheValue:
      return true;
  }
  throw InternalError("TypeStatesItsRendering: unknown pattern rendering");
}

}  // namespace lyra::lowering::hir_to_mir
