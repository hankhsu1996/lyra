#include "render_link_progress.hpp"

#include <iterator>
#include <string_view>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "text_sink.hpp"

namespace lyra::driver {

namespace {

auto LinkProgressPhaseName(lowering::mir_to_llvm::LinkProgressPhase phase)
    -> std::string_view {
  using enum lowering::mir_to_llvm::LinkProgressPhase;
  switch (phase) {
    case kCodegen:
      return "codegen";
    case kLinkGraph:
      return "link_graph";
    case kLinkAlloc:
      return "link_alloc";
    case kLinkFixup:
      return "link_fixup";
    case kFinalize:
      return "finalize";
  }
  throw common::InternalError(
      "LinkProgressPhaseName", "invalid link progress phase");
}

}  // namespace

void RenderLinkProgress(
    TextSink& sink, const lowering::mir_to_llvm::LinkProgressSnapshot& snap) {
  auto phase_name = LinkProgressPhaseName(snap.phase);
  fmt::memory_buffer buf;
  fmt::format_to(
      std::back_inserter(buf), "[lyra][jit][progress] {:.1f}s phase={}",
      snap.elapsed_seconds, phase_name);
  if (snap.object_count > 0) {
    fmt::format_to(
        std::back_inserter(buf), " objects={} obj_bytes={}", snap.object_count,
        snap.total_object_bytes);
  }
  if (snap.relocation_count > 0) {
    fmt::format_to(
        std::back_inserter(buf), " relocs={} syms={} sections={} blocks={}",
        snap.relocation_count, snap.symbol_count, snap.section_count,
        snap.block_count);
  }
  if (snap.codegen_seconds > 0.0) {
    fmt::format_to(
        std::back_inserter(buf), " codegen={:.3f}s", snap.codegen_seconds);
  }
  if (snap.link_graph_seconds > 0.0) {
    fmt::format_to(
        std::back_inserter(buf), " link_graph={:.3f}s",
        snap.link_graph_seconds);
  }
  if (snap.link_alloc_seconds > 0.0) {
    fmt::format_to(
        std::back_inserter(buf), " link_alloc={:.3f}s",
        snap.link_alloc_seconds);
  }
  if (snap.link_fixup_seconds > 0.0) {
    fmt::format_to(
        std::back_inserter(buf), " link_fixup={:.3f}s",
        snap.link_fixup_seconds);
  }
  fmt::format_to(std::back_inserter(buf), "\n");
  sink.Write(std::string_view(buf.data(), buf.size()));
}

}  // namespace lyra::driver
