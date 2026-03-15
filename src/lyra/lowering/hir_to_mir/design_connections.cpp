#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/compiled_bindings.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/sensitivity.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

auto MakeDesignContext(
    const LoweringInput& input, mir::Arena& mir_arena,
    const DesignDeclarations& decls) -> Context {
  // Connection processes are design-level: they only see design-global places.
  // No body_places -- connections operate across instances.
  return Context{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .design_places = &decls.design_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = &decls.functions,
      .return_slot = std::nullopt,
      .return_type = input.builtin_types.void_type,
      .design_slots = &decls.slots,
  };
}

auto CreateConnectionProcess(
    mir::PlaceId dst, BuildSrcFn build_src, const LoweringInput& input,
    mir::Arena& mir_arena, const DesignDeclarations& decls)
    -> Result<mir::Process> {
  Context ctx = MakeDesignContext(input, mir_arena, decls);
  MirBuilder builder(&mir_arena, &ctx, nullptr, hir::kInvalidModuleBodyId);

  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  // Build source operand INSIDE this builder (captures any temps/emissions)
  auto src_result = build_src(builder, ctx);
  if (!src_result) {
    return std::unexpected(src_result.error());
  }

  builder.EmitAssign(dst, *src_result);
  builder.EmitRepeat();

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // Collect sensitivity and replace Repeat with Wait
  mir::Process temp_process{
      .kind = mir::ProcessKind::kLooping,
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = blocks,
      .temp_metadata = {},
  };
  auto triggers = mir::CollectSensitivity(temp_process, mir_arena);

  for (auto& block : blocks) {
    if (std::holds_alternative<mir::Repeat>(block.terminator.data)) {
      block.terminator.data = mir::Wait{
          .triggers = std::move(triggers),
          .resume = mir::BasicBlockId{entry_idx.value},
      };
      break;
    }
  }

  return mir::Process{
      .kind = mir::ProcessKind::kLooping,
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .temp_metadata = std::move(ctx.temp_metadata),
  };
}

namespace {

// Compile kDriveParentToChild binding: parent rvalue -> child place via
// process.
auto CompileDriveParentToChild(
    const ast_to_hir::PortBinding& binding, mir::PlaceId child_place,
    const DesignDeclarations& decls, const LoweringInput& input,
    mir::Arena& mir_arena) -> Result<mir::CompiledDriveBinding> {
  // Source is parent rvalue expression (arbitrary, may emit temps)
  auto build_src =
      [&binding](MirBuilder& builder, const Context&) -> Result<mir::Operand> {
    return LowerExpression(binding.parent_rvalue, builder);
  };

  auto proc =
      CreateConnectionProcess(child_place, build_src, input, mir_arena, decls);
  if (!proc) {
    return std::unexpected(proc.error());
  }

  return mir::CompiledDriveBinding{
      .kind = mir::PortConnection::Kind::kDriveParentToChild,
      .child_port_sym = binding.child_port_sym,
      .parent_instance_sym = binding.parent_instance_sym,
      .child_place = child_place,
      .parent_place = {},
      .body = {.process = std::move(*proc)},
  };
}

// Compile kDriveChildToParent binding: child place -> parent place via process.
auto CompileDriveChildToParent(
    const ast_to_hir::PortBinding& binding, mir::PlaceId child_place,
    const DesignDeclarations& decls, const LoweringInput& input,
    mir::Arena& mir_arena) -> Result<mir::CompiledDriveBinding> {
  // Lower parent lvalue -> place (pure by construction - no MirBuilder)
  Context ctx = MakeDesignContext(input, mir_arena, decls);
  auto parent_lv = LowerPureLvaluePlace(binding.parent_lvalue, ctx);
  if (!parent_lv) {
    return std::unexpected(parent_lv.error());
  }

  // Validate parent place resolves to kDesignGlobal (may have projections)
  const mir::Place& parent_place = mir_arena[*parent_lv];
  if (parent_place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal) {
    throw common::InternalError(
        "CompileDriveChildToParent", "output target must be a design slot");
  }

  // Source is explicit read from child place (value, not handle)
  mir::PlaceId captured_child = child_place;
  auto build_src = [captured_child](
                       MirBuilder&, const Context&) -> Result<mir::Operand> {
    return mir::Operand::Use(captured_child);
  };

  auto proc =
      CreateConnectionProcess(*parent_lv, build_src, input, mir_arena, decls);
  if (!proc) {
    return std::unexpected(proc.error());
  }

  return mir::CompiledDriveBinding{
      .kind = mir::PortConnection::Kind::kDriveChildToParent,
      .child_port_sym = binding.child_port_sym,
      .parent_instance_sym = binding.parent_instance_sym,
      .child_place = child_place,
      .parent_place = *parent_lv,
      .body = {.process = std::move(*proc)},
  };
}

}  // namespace

auto CompileBindings(
    const ast_to_hir::DesignBindingPlan& plan, const DesignDeclarations& decls,
    const LoweringInput& input, mir::Arena& mir_arena)
    -> Result<mir::CompiledBindingPlan> {
  mir::CompiledBindingPlan result;
  if (plan.bindings.empty()) {
    return result;
  }

  for (const auto& binding : plan.bindings) {
    mir::PlaceId child_place = decls.design_places.at(binding.child_port_sym);

    switch (binding.kind) {
      case ast_to_hir::PortBinding::Kind::kDriveParentToChild: {
        auto compiled = CompileDriveParentToChild(
            binding, child_place, decls, input, mir_arena);
        if (!compiled) return std::unexpected(compiled.error());
        result.drive_bindings.push_back(std::move(*compiled));
        break;
      }
      case ast_to_hir::PortBinding::Kind::kDriveChildToParent: {
        auto compiled = CompileDriveChildToParent(
            binding, child_place, decls, input, mir_arena);
        if (!compiled) return std::unexpected(compiled.error());
        result.drive_bindings.push_back(std::move(*compiled));
        break;
      }
    }
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
