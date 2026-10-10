#include "lyra/lowering/hir_to_mir/statement/foreach.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/flow.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A dimension whose indices its declaration fixes (LRM 7.4): they run from
// `left` to `right`, in whichever direction that is.
struct DeclaredRange {
  std::int64_t left = 0;
  std::int64_t right = 0;
};

// A dimension whose indices are 0 up to a count the running program holds: a
// dynamic array's or a queue's size (LRM 7.5, 7.10), a string's length (LRM
// 6.16). `count_entry` is what answers with it.
struct CountedWhileRunning {
  support::BuiltinFn count_entry;
};

// A dimension whose indices are the keys an associative array holds (LRM 7.8),
// visited in the order its index type keeps them.
struct KeysHeld {};

using IndexSet = std::variant<DeclaredRange, CountedWhileRunning, KeysHeld>;

// One dimension of an iterated type: the indices it has, and the type of one
// element of it, absent where the type names none -- a character of a string,
// a bit of an integral type that is no array.
struct Dimension {
  IndexSet indices;
  std::optional<hir::TypeId> element;
};

// The outermost dimension of `type` as a `foreach` iterates it (LRM 12.7.3).
// An integral type that is no array is iterated as the vector of bits it is
// (LRM 6.11.1), numbered from its most significant bit down to 0.
auto DimensionOf(const UnitLowerer& unit_lowerer, hir::TypeId type)
    -> Dimension {
  const auto whole_vector = [&]() -> Dimension {
    const std::uint64_t width = unit_lowerer.Unit()
                                    .types.Get(unit_lowerer.TranslateType(type))
                                    .Integral()
                                    .bit_width;
    return Dimension{
        .indices =
            DeclaredRange{
                .left = static_cast<std::int64_t>(width) - 1, .right = 0},
        .element = std::nullopt};
  };
  const auto none = []() -> Dimension {
    throw InternalError(
        "DimensionOf: a foreach names a variable for a dimension its array "
        "does not have, which the front end rejects (LRM 12.7.3)");
  };
  return unit_lowerer.Hir().types.Get(type).Visit(
      Overloaded{
          [&](const hir::PackedArrayType& t) {
            return Dimension{
                .indices =
                    DeclaredRange{.left = t.dim.left, .right = t.dim.right},
                .element = t.element_type};
          },
          [&](const hir::UnpackedArrayType& t) {
            return Dimension{
                .indices =
                    DeclaredRange{.left = t.dim.left, .right = t.dim.right},
                .element = t.element_type};
          },
          [&](const hir::DynamicArrayType& t) {
            return Dimension{
                .indices =
                    CountedWhileRunning{
                        .count_entry = support::BuiltinFn::kSize},
                .element = t.element_type};
          },
          [&](const hir::QueueType& t) {
            return Dimension{
                .indices =
                    CountedWhileRunning{
                        .count_entry = support::BuiltinFn::kSize},
                .element = t.element_type};
          },
          [&](const hir::AssociativeArrayType& t) {
            return Dimension{.indices = KeysHeld{}, .element = t.element_type};
          },
          [&](const hir::StringType&) {
            return Dimension{
                .indices =
                    CountedWhileRunning{
                        .count_entry = support::BuiltinFn::kLen},
                .element = std::nullopt};
          },
          [&](const hir::PackedStructType&) { return whole_vector(); },
          [&](const hir::PackedUnionType&) { return whole_vector(); },
          [&](const hir::EnumType&) { return whole_vector(); },
          [&](const hir::ScalarBitType&) { return none(); },
          [&](const hir::UnpackedStructType&) { return none(); },
          [&](const hir::UnpackedUnionType&) { return none(); },
          [&](const hir::WildcardIndexType&) { return none(); },
          [&](const hir::EventType&) { return none(); },
          [&](const hir::RealType&) { return none(); },
          [&](const hir::ShortRealType&) { return none(); },
          [&](const hir::RealTimeType&) { return none(); },
          [&](const hir::ChandleType&) { return none(); },
          [&](const hir::ClassHandleType&) { return none(); },
          [&](const hir::ImportedClassHandleType&) { return none(); },
          [&](const hir::UnitObjectType&) { return none(); },
          [&](const hir::UnitObjectsType&) { return none(); },
          [&](const hir::VirtualInterfaceType&) { return none(); },
          [&](const hir::NullType&) { return none(); },
          [&](const hir::VoidType&) { return none(); }});
}

// Whether iterating the dimension reads the array while the program runs.
auto ReadsTheArray(const IndexSet& indices) -> bool {
  return std::visit(
      Overloaded{
          [](const DeclaredRange&) { return false; },
          [](const CountedWhileRunning&) { return true; },
          [](const KeysHeld&) { return true; }},
      indices);
}

// One iterated dimension: how many dimensions of the array lie above it,
// iterated or not, the type it is the outermost dimension of, the indices it
// has, and its loop variable with the type of the value that variable holds.
// `element_type` is the type of one element of the dimension, absent where the
// iterated type names none.
struct Level {
  std::size_t position = 0;
  hir::TypeId container;
  IndexSet indices;
  std::optional<mir::TypeId> element_type;
  hir::ProceduralVarId var;
  mir::TypeId index_type;
};

// A `foreach` while its loops are built: the iterated dimensions outermost
// first, and the array as a path settled in the implicit block around the
// loops (LRM 12.7.3), absent where no dimension reads the array while the
// program runs. `leaves` is the label the outermost loop carries, which a
// `break` in the body names to leave every dimension (LRM 12.8).
struct Nest {
  ProcessLowerer* process = nullptr;
  const hir::ForeachStmt* stmt = nullptr;
  std::vector<Level> levels;
  std::optional<SettledPath> array;
  mir::LoopLabelId leaves;
};

// The source's body, lowered into a scope of its own.
auto LowerBody(const Nest& nest, const WalkFrame& frame)
    -> diag::Result<mir::Block> {
  return LowerStmtIntoChildScope(
      *nest.process, frame.WithBreakLeaving(nest.leaves), nest.stmt->body);
}

// The loop variable of `level`, as the path to the storage its declaration gave
// it, named in the frame's block. The path evaluates nothing.
auto IndexPath(const Nest& nest, const WalkFrame& frame, const Level& level)
    -> AccessPath {
  return AccessPath{
      .owner = frame.current_block->exprs.Add(
          LowerProceduralVarRefExpr(*nest.process, frame, level.var)),
      .descent = {}};
}

// The value the level at `depth` iterates, read in the frame's block: the array
// itself for the outermost dimension, and for one inside it the element the
// enclosing loop variables select. It is read where the level's loop stands,
// inside every enclosing loop, which is what gives each row of a jagged array
// its own bound.
auto IteratedValue(const Nest& nest, const WalkFrame& frame, std::size_t depth)
    -> mir::ExprId {
  UnitLowerer& unit_lowerer = nest.process->Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  if (!nest.array.has_value() || nest.levels[depth].position != depth) {
    throw InternalError(
        "IteratedValue: a dimension read while the program runs lies under a "
        "dimension the foreach skips, which leaves no index to reach it by "
        "and which the front end rejects (LRM 12.7.3)");
  }
  AccessPath reached = NamedIn(*nest.array, block);
  for (std::size_t outer = 0; outer < depth; ++outer) {
    const Level& enclosing = nest.levels[outer];
    if (!enclosing.element_type.has_value()) {
      throw InternalError(
          "IteratedValue: a dimension with another inside it has elements");
    }
    const mir::ExprId index =
        PathValue(unit, block, IndexPath(nest, frame, enclosing));
    reached = DescendInto(
        std::move(reached), ElementStep(
                                unit_lowerer, block, enclosing.container, index,
                                *enclosing.element_type));
  }
  return PathValue(unit, block, reached);
}

// The loop over an associative dimension (LRM 7.9.4, 7.9.6): a `for` whose
// counter holds what `first` and then `next` answer, and whose every step
// leaves the key it visited in the loop variable. An array holding no entry
// answers 0 to `first`, so the body never runs, and a `continue` lands on the
// step, which is what advances the key (LRM 12.8). The `for` declares the
// counter; the key is the loop variable, which the traversal writes.
auto BuildKeyLoopStmt(
    const Nest& nest, const WalkFrame& frame, std::size_t depth,
    mir::BlockId body_scope, std::optional<mir::LoopLabelId> break_label)
    -> mir::Stmt {
  UnitLowerer& unit_lowerer = nest.process->Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const Level& level = nest.levels[depth];
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::LocalId more = frame.bindings->DeclareAnonymous(int_type);

  const auto traversal = [&](support::BuiltinFn method) {
    BlockBuilder steps(frame);
    const mir::ExprId array = IteratedValue(nest, steps.Frame(), depth);
    return block.exprs.Add(BuildAssociativeTraversal(
        unit_lowerer, steps, method, array,
        IndexPath(nest, steps.Frame(), level), level.index_type, int_type));
  };
  const mir::ExprId first = traversal(support::BuiltinFn::kAssocFirst);
  const mir::ExprId next = traversal(support::BuiltinFn::kAssocNext);
  const mir::ExprId advance = block.exprs.Add(
      mir::MakeAssignExpr(
          unit.builtins, block.exprs.Add(mir::MakeLocalRefExpr(more, int_type)),
          next));

  std::vector<mir::ForInit> init;
  init.emplace_back(mir::ForInitDecl{.induction_var = more, .init = first});
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ForStmt{
          .init = std::move(init),
          .condition = ReduceToCondition(
              unit, block,
              block.exprs.Add(mir::MakeLocalRefExpr(more, int_type))),
          .step = {advance},
          .scope = body_scope,
          .break_label = break_label}};
}

// The loop of the level at `depth`, standing in the frame's block: its body is
// the loop of the level inside it, and the innermost level's body is the
// source's own. A count the loop samples ahead of itself is appended to that
// block, whose statements therefore run before the returned loop. `break_label`
// is the label this loop carries, which only the outermost does.
auto BuildLevelLoopStmt(
    const Nest& nest, const WalkFrame& frame, std::size_t depth,
    std::optional<mir::LoopLabelId> break_label) -> diag::Result<mir::Stmt> {
  ProcessLowerer& process = *nest.process;
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;

  mir::Block pass;
  if (depth + 1 == nest.levels.size()) {
    auto body = LowerBody(nest, frame);
    if (!body) return std::unexpected(std::move(body.error()));
    pass = *std::move(body);
  } else {
    auto inner = BuildLevelLoopStmt(
        nest, frame.WithBlock(&pass), depth + 1, std::nullopt);
    if (!inner) return std::unexpected(std::move(inner.error()));
    pass.AppendStmt(*std::move(inner));
  }
  const mir::BlockId body_scope = block.child_scopes.Add(std::move(pass));

  const Level& level = nest.levels[depth];
  const auto require_int_index = [&] {
    if (level.index_type != unit.builtins.int_type) {
      throw InternalError(
          "BuildLevelLoopStmt: the loop variable of a dimension indexed by "
          "position is an int (LRM 12.7.3)");
    }
  };
  return std::visit(
      Overloaded{
          [&](const DeclaredRange& range) {
            require_int_index();
            return BuildRangeLoopStmt(
                unit, block, IndexPath(nest, frame, level), range.left,
                range.right, body_scope, break_label);
          },
          [&](const CountedWhileRunning& counted) {
            require_int_index();
            // The bound is read once, on entry to the dimension, the way a
            // repeat-loop's count is (LRM 12.7.3).
            const mir::ExprId count = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target = counted.count_entry,
                                    .receiver =
                                        IteratedValue(nest, frame, depth)},
                            .arguments = {}},
                    .type = unit.builtins.int_type});
            return BuildCountingLoopStmt(
                unit, frame, block, count, IndexPath(nest, frame, level),
                body_scope, break_label);
          },
          [&](const KeysHeld&) {
            return BuildKeyLoopStmt(
                nest, frame, depth, body_scope, break_label);
          }},
      level.indices);
}

}  // namespace

auto LowerForeachStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForeachStmt& f) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::ProceduralBody& hir_body = process.HirBody();
  const hir::Expr& array = hir_body.exprs.Get(f.array);

  std::vector<Level> levels;
  std::optional<hir::TypeId> iterated = array.type;
  bool reads_array = false;
  for (std::size_t position = 0; position < f.loop_vars.size(); ++position) {
    if (!iterated.has_value()) {
      throw InternalError(
          "LowerForeachStmt: a foreach lists more dimensions than its array "
          "has, which the front end rejects (LRM 12.7.3)");
    }
    const hir::TypeId container = *iterated;
    const Dimension dimension = DimensionOf(unit_lowerer, container);
    iterated = dimension.element;
    if (!f.loop_vars[position].has_value()) {
      continue;
    }
    const hir::ProceduralVarId var = *f.loop_vars[position];
    reads_array = reads_array || ReadsTheArray(dimension.indices);
    levels.push_back(
        Level{
            .position = position,
            .container = container,
            .indices = dimension.indices,
            .element_type = dimension.element.has_value()
                                ? std::optional{unit_lowerer.TranslateType(
                                      *dimension.element)}
                                : std::nullopt,
            .var = var,
            .index_type = unit_lowerer.TranslateType(
                hir_body.procedural_vars.Get(var).type)});
  }

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // LRM 12.7.3 makes each loop variable a variable of the implicit block around
  // the loop, so each is declared where that block is entered, ahead of every
  // loop, the way any variable a block declares is. One a detached fork branch
  // still reads after the loop ends is lifted as any such variable is (LRM
  // 6.21).
  std::vector<hir::ProceduralVarId> loop_vars;
  loop_vars.reserve(levels.size());
  for (const Level& level : levels) {
    loop_vars.push_back(level.var);
  }
  OpenActivationScope(process, wrapper_frame, loop_vars);
  for (const hir::ProceduralVarId var : loop_vars) {
    auto declared = LowerVarDeclaration(process, wrapper_frame, var);
    if (!declared) return std::unexpected(std::move(declared.error()));
    wrapper.AppendStmt(*std::move(declared));
  }

  // The source names the array once, so what reaching it computes is evaluated
  // once, ahead of every loop, and each dimension that reads it names the
  // result.
  std::optional<SettledPath> settled_array;
  if (reads_array) {
    auto path = process.LowerAccessPath(array, wrapper_frame);
    if (!path) return std::unexpected(std::move(path.error()));
    settled_array =
        SettledForRead(unit_lowerer, wrapper_frame, *std::move(path));
  }

  const Nest nest{
      .process = &process,
      .stmt = &f,
      .levels = std::move(levels),
      .array = std::move(settled_array),
      .leaves = process.NextLoopLabel()};

  if (nest.levels.empty()) {
    // A list naming no variable iterates no dimension, so the body runs once.
    // It is still the body of a loop, which is what a `break` or a `continue`
    // written in it binds to (LRM 12.8).
    auto body = LowerBody(nest, wrapper_frame);
    if (!body) return std::unexpected(std::move(body.error()));
    const mir::BlockId body_scope = wrapper.child_scopes.Add(*std::move(body));
    wrapper.AppendStmt(BuildCountingLoopStmt(
        unit, wrapper_frame, wrapper, BuildIntLiteral(unit, wrapper, 1),
        wrapper_frame.bindings->DeclareAnonymous(unit.builtins.int_type),
        body_scope, nest.leaves));
  } else {
    auto outermost = BuildLevelLoopStmt(nest, wrapper_frame, 0, nest.leaves);
    if (!outermost) return std::unexpected(std::move(outermost.error()));
    wrapper.AppendStmt(*std::move(outermost));
  }

  const mir::BlockId scope =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope}};
}

}  // namespace lyra::lowering::hir_to_mir
