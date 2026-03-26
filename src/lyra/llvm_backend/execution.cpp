#include "lyra/llvm_backend/execution.hpp"

#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <utility>

#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/ir_optimize.hpp"
#include "lyra/llvm_backend/lower.hpp"

namespace lyra::lowering::mir_to_llvm {

struct JitSession::Impl {
  std::unique_ptr<llvm::orc::LLJIT> jit;
  int (*entry_fn)() = nullptr;
  JitCompileTimings timings;
  JitOrcStats orc_stats;
};

JitSession::JitSession() = default;
JitSession::~JitSession() = default;
JitSession::JitSession(JitSession&&) noexcept = default;
auto JitSession::operator=(JitSession&&) noexcept -> JitSession& = default;

auto JitSession::Run() -> int {
  return impl_->entry_fn();
}

auto JitSession::Timings() const -> const JitCompileTimings& {
  return impl_->timings;
}

auto JitSession::OrcStats() const -> const JitOrcStats& {
  return impl_->orc_stats;
}

namespace {

auto ToCodeGenOpt(OptLevel level) -> llvm::CodeGenOpt::Level {
  switch (level) {
    case OptLevel::kO0:
      return llvm::CodeGenOpt::None;
    case OptLevel::kO1:
      return llvm::CodeGenOpt::Less;
    case OptLevel::kO2:
      return llvm::CodeGenOpt::Default;
    case OptLevel::kO3:
      return llvm::CodeGenOpt::Aggressive;
  }
  throw common::InternalError("ToCodeGenOpt", "unknown OptLevel");
}

using Clock = std::chrono::steady_clock;

auto ElapsedSeconds(Clock::time_point start) -> double {
  return std::chrono::duration<double>(Clock::now() - start).count();
}

// Shared progress state for JIT link profiling.
// Updated incrementally by ObjectTransformLayer callback and ProfilingPlugin.
// Read by the optional watchdog thread during lookup.
struct LinkProgress {
  mutable std::mutex mutex;
  std::atomic<bool> stop{false};

  // Set before lookup starts (no mutex needed for reads).
  Clock::time_point lookup_start;
  std::string linker_backend;

  // Updated by ObjectTransformLayer (mutex-protected).
  Clock::time_point codegen_end;
  int object_count = 0;
  int64_t total_object_bytes = 0;

  // Updated by ProfilingPlugin (mutex-protected).
  // Timestamps: overwritten per object (latest wins).
  // Counters: aggregated across objects (+=).
  Clock::time_point graph_ready;
  Clock::time_point alloc_done;
  Clock::time_point fixup_done;
  Clock::time_point emitted;  // After finalization (notifyEmitted callback)
  int relocation_count = 0;
  int symbol_count = 0;
  int section_count = 0;
  int block_count = 0;

  // Finalization diagnostics: collected in PostFixupPasses (just before
  // finalize). Segments = distinct {MemProt, MemLifetimePolicy} groups,
  // each receiving one mprotect call.  Bytes = total block content.
  int finalize_segments = 0;
  int64_t finalize_bytes = 0;
};

// JITLink plugin that records sub-phase timings and graph counters.
class ProfilingPlugin : public llvm::orc::ObjectLinkingLayer::Plugin {
 public:
  explicit ProfilingPlugin(LinkProgress* progress) : progress_(progress) {
  }

  void modifyPassConfig(
      llvm::orc::MaterializationResponsibility& mr,
      llvm::jitlink::LinkGraph& lg,
      llvm::jitlink::PassConfiguration& config) override {
    (void)mr;
    (void)lg;
    {
      std::lock_guard lock(progress_->mutex);
      progress_->graph_ready = Clock::now();
      progress_->alloc_done = {};
      progress_->fixup_done = {};
    }

    // PrePrune: count raw graph stats before dead-stripping.
    config.PrePrunePasses.push_back(
        [this](llvm::jitlink::LinkGraph& g) -> llvm::Error {
          std::lock_guard lock(progress_->mutex);
          progress_->section_count += static_cast<int>(g.sections_size());
          for (const auto* block : g.blocks()) {
            ++progress_->block_count;
            for (const auto& edge : block->edges()) {
              if (edge.isRelocation()) ++progress_->relocation_count;
            }
          }
          for ([[maybe_unused]] const auto& sym : g.defined_symbols())
            ++progress_->symbol_count;
          for ([[maybe_unused]] const auto& sym : g.external_symbols())
            ++progress_->symbol_count;
          return llvm::Error::success();
        });

    config.PostAllocationPasses.push_back(
        [this](llvm::jitlink::LinkGraph&) -> llvm::Error {
          std::lock_guard lock(progress_->mutex);
          progress_->alloc_done = Clock::now();
          return llvm::Error::success();
        });

    config.PostFixupPasses.push_back(
        [this](llvm::jitlink::LinkGraph& g) -> llvm::Error {
          std::lock_guard lock(progress_->mutex);
          progress_->fixup_done = Clock::now();

          // Count distinct memory segments (protection groups) that will each
          // get a separate mprotect call during finalization.  This mirrors
          // BasicLayout's segment grouping: {MemProt, MemLifetimePolicy}.
          // Also accumulate total block bytes per segment.
          uint32_t seen_groups = 0;  // Bit-set over AllocGroup::Id (max 32)
          for (const auto& sec : g.sections()) {
            if (sec.blocks().empty()) continue;
            auto mlp =
                sec.getMemLifetimePolicy();  // NOLINT(misc-include-cleaner)
            if (mlp == llvm::orc::MemLifetimePolicy::NoAlloc)
              continue;  // NOLINT(misc-include-cleaner)
            uint8_t group_id =
                static_cast<uint8_t>(sec.getMemProt()) |
                (static_cast<uint8_t>(static_cast<bool>(mlp)) << 3);
            seen_groups |= (1U << group_id);
            for (const auto* block : sec.blocks()) {
              progress_->finalize_bytes +=
                  static_cast<int64_t>(block->getSize());
            }
          }
          progress_->finalize_segments += __builtin_popcount(seen_groups);
          return llvm::Error::success();
        });
  }

  auto notifyEmitted(llvm::orc::MaterializationResponsibility& mr)
      -> llvm::Error override {
    (void)mr;
    std::lock_guard lock(progress_->mutex);
    progress_->emitted = Clock::now();
    return llvm::Error::success();
  }

  auto notifyFailed(llvm::orc::MaterializationResponsibility& mr)
      -> llvm::Error override {
    (void)mr;
    return llvm::Error::success();
  }

  auto notifyRemovingResources(
      llvm::orc::JITDylib& jd, llvm::orc::ResourceKey key)
      -> llvm::Error override {
    (void)jd;
    (void)key;
    return llvm::Error::success();
  }

  void notifyTransferringResources(
      llvm::orc::JITDylib& jd, llvm::orc::ResourceKey dst_key,
      llvm::orc::ResourceKey src_key) override {
    (void)jd;
    (void)dst_key;
    (void)src_key;
  }

 private:
  LinkProgress* progress_;
};

auto SnapshotLinkProgress(LinkProgress& progress) -> LinkProgressSnapshot {
  auto now = Clock::now();
  double elapsed =
      std::chrono::duration<double>(now - progress.lookup_start).count();

  auto phase = LinkProgressPhase::kCodegen;
  if (progress.codegen_end != Clock::time_point{}) {
    phase = LinkProgressPhase::kLinkGraph;
    if (progress.graph_ready != Clock::time_point{}) {
      phase = LinkProgressPhase::kLinkAlloc;
      if (progress.alloc_done != Clock::time_point{}) {
        phase = LinkProgressPhase::kLinkFixup;
        if (progress.fixup_done != Clock::time_point{}) {
          phase = LinkProgressPhase::kFinalize;
        }
      }
    }
  }

  auto dur = [&](Clock::time_point from, Clock::time_point to) -> double {
    if (from == Clock::time_point{} || to == Clock::time_point{}) return 0.0;
    return std::chrono::duration<double>(to - from).count();
  };

  return LinkProgressSnapshot{
      .elapsed_seconds = elapsed,
      .phase = phase,
      .object_count = progress.object_count,
      .total_object_bytes = progress.total_object_bytes,
      .relocation_count = progress.relocation_count,
      .symbol_count = progress.symbol_count,
      .section_count = progress.section_count,
      .block_count = progress.block_count,
      .codegen_seconds = dur(progress.lookup_start, progress.codegen_end),
      .link_graph_seconds = dur(progress.codegen_end, progress.graph_ready),
      .link_alloc_seconds = dur(progress.graph_ready, progress.alloc_done),
      .link_fixup_seconds = dur(progress.alloc_done, progress.fixup_done),
  };
}

// Emits structured JIT link-progress snapshots every 5 seconds until stopped.
// Uses short sleep intervals so join() returns promptly after stop is set.
void RunLinkWatchdog(
    LinkProgress& progress, const LinkProgressReporter& reporter) {
  constexpr int kIntervalMs = 5000;
  constexpr int kPollMs = 100;
  int ms_since_report = 0;
  while (!progress.stop.load(std::memory_order_relaxed)) {
    std::this_thread::sleep_for(std::chrono::milliseconds(kPollMs));
    if (progress.stop.load(std::memory_order_relaxed)) break;

    ms_since_report += kPollMs;
    if (ms_since_report < kIntervalMs) continue;
    ms_since_report = 0;

    LinkProgressSnapshot snapshot;
    {
      std::lock_guard lock(progress.mutex);
      snapshot = SnapshotLinkProgress(progress);
    }
    reporter(snapshot);
  }
}

void InitializeLlvm() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

// Core JIT compilation logic. Returns the LLJIT instance and entry function.
// Caller is responsible for wrapping into JitSession.
struct CompileResult {
  std::unique_ptr<llvm::orc::LLJIT> jit;
  int (*entry_fn)() = nullptr;
  JitCompileTimings timings;
  JitOrcStats orc_stats;
};

auto CompileJitImpl(
    LoweringResult& result,
    const std::optional<std::filesystem::path>& runtime_path,
    const JitCompileOptions& options)
    -> std::expected<CompileResult, std::string> {
  bool profiling = options.enable_profiling || options.progress_reporter;
  JitCompileTimings timings;

  // Sub-phase 1: create_jit
  auto t0 = Clock::now();
  InitializeLlvm();

  auto jtmb = llvm::orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb) {
    return std::unexpected(
        std::format(
            "failed to detect host: {}", llvm::toString(jtmb.takeError())));
  }
  jtmb->setCodeGenOptLevel(ToCodeGenOpt(options.opt_level));
  auto jit = llvm::orc::LLJITBuilder()
                 .setJITTargetMachineBuilder(std::move(*jtmb))
                 .create();
  if (!jit) {
    return std::unexpected(
        std::format(
            "failed to create JIT: {}", llvm::toString(jit.takeError())));
  }
  timings.create_jit = ElapsedSeconds(t0);

  // Sub-phase 2: load_runtime
  auto t1 = Clock::now();
  auto& dylib = (*jit)->getMainJITDylib();
  if (runtime_path) {
    auto gen = llvm::orc::DynamicLibrarySearchGenerator::Load(
        runtime_path->string().c_str(),
        (*jit)->getDataLayout().getGlobalPrefix());
    if (!gen) {
      return std::unexpected(
          std::format(
              "failed to load runtime '{}': {}", runtime_path->string(),
              llvm::toString(gen.takeError())));
    }
    dylib.addGenerator(std::move(*gen));
  } else {
    auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        (*jit)->getDataLayout().getGlobalPrefix());
    if (!gen) {
      return std::unexpected(
          std::format(
              "failed to get host process symbols: {}",
              llvm::toString(gen.takeError())));
    }
    dylib.addGenerator(std::move(*gen));
  }

  // JIT materialization: load each validated DPI link input as a dynamic
  // library into the JIT symbol resolver.
  for (size_t i = 0; i < options.dpi_link_inputs.size(); ++i) {
    auto dpi_path = options.dpi_link_inputs[i].string();
    auto dpi_gen = llvm::orc::DynamicLibrarySearchGenerator::Load(
        dpi_path.c_str(), (*jit)->getDataLayout().getGlobalPrefix());
    if (!dpi_gen) {
      return std::unexpected(
          std::format(
              "failed to dynamically load DPI link input #{} '{}': {}", i,
              dpi_path, llvm::toString(dpi_gen.takeError())));
    }
    dylib.addGenerator(std::move(*dpi_gen));
  }

  // Validate module DataLayout matches JIT target.
  // DataLayout MUST be set before lowering (in LowerMirToLlvm) - never mutate
  // it here. Post-lowering mutation would invalidate DL-dependent constants
  // already baked into the IR (e.g., memset sizes, struct offsets).
  const auto& module_dl = result.module->getDataLayout();
  const auto& jit_dl = (*jit)->getDataLayout();
  if (module_dl.isDefault()) {
    throw common::InternalError(
        "CompileJitImpl", "module DataLayout not set before lowering");
  }
  if (module_dl != jit_dl) {
    throw common::InternalError(
        "CompileJitImpl",
        std::format(
            "module DataLayout mismatch: module='{}', jit='{}'",
            module_dl.getStringRepresentation(),
            jit_dl.getStringRepresentation()));
  }
  timings.load_runtime = ElapsedSeconds(t1);

  // Install profiling instrumentation when enabled.
  std::optional<LinkProgress> progress;
  if (profiling) {
    progress.emplace();
    if (auto* oll = llvm::dyn_cast<llvm::orc::ObjectLinkingLayer>(
            &(*jit)->getObjLinkingLayer())) {
      progress->linker_backend = "JITLink";
      oll->addPlugin(std::make_unique<ProfilingPlugin>(&*progress));
    } else {
      progress->linker_backend = "RTDyld";
    }

    // Object transform measures codegen->link boundary.
    (*jit)->getObjTransformLayer().setTransform(
        [&progress](std::unique_ptr<llvm::MemoryBuffer> buf)
            -> llvm::Expected<std::unique_ptr<llvm::MemoryBuffer>> {
          std::lock_guard lock(progress->mutex);
          progress->object_count++;
          progress->total_object_bytes +=
              static_cast<int64_t>(buf->getBufferSize());
          progress->codegen_end = Clock::now();
          return buf;
        });
  }

  // Sub-phase 3: optimize_ir
  auto t2 = Clock::now();
  OptimizeModule(*result.module, options.opt_level);
  timings.optimize_ir = ElapsedSeconds(t2);

  // Sub-phase 4: add_ir
  auto t3 = Clock::now();
  llvm::orc::ThreadSafeContext tsc(std::move(result.context));
  auto tsm = llvm::orc::ThreadSafeModule(std::move(result.module), tsc);
  if (auto err = (*jit)->addIRModule(std::move(tsm))) {
    return std::unexpected(
        std::format(
            "failed to add module: {}", llvm::toString(std::move(err))));
  }
  timings.add_ir = ElapsedSeconds(t3);

  // Sub-phase 5: lookup_main (triggers full JIT compilation + linking)
  if (progress) {
    progress->lookup_start = Clock::now();
  }

  std::optional<std::thread> watchdog;
  if (options.progress_reporter && progress) {
    watchdog.emplace([&progress, &options] {
      RunLinkWatchdog(*progress, options.progress_reporter);
    });
  }

  auto main_sym = (*jit)->lookup("main");

  if (progress) {
    progress->stop.store(true, std::memory_order_relaxed);
  }
  if (watchdog) watchdog->join();

  if (!main_sym) {
    return std::unexpected(
        std::format(
            "symbol 'main' not found: {}",
            llvm::toString(main_sym.takeError())));
  }

  // Compute timings from profiling data (single-threaded after join, no lock).
  auto lookup_end = Clock::now();
  JitOrcStats orc_stats;

  if (progress) {
    timings.lookup_main =
        std::chrono::duration<double>(lookup_end - progress->lookup_start)
            .count();
    if (progress->object_count > 0) {
      timings.codegen = std::chrono::duration<double>(
                            progress->codegen_end - progress->lookup_start)
                            .count();
      timings.linking = timings.lookup_main - timings.codegen;
    }

    if (progress->fixup_done != Clock::time_point{}) {
      timings.link_graph = std::chrono::duration<double>(
                               progress->graph_ready - progress->codegen_end)
                               .count();
      timings.link_alloc = std::chrono::duration<double>(
                               progress->alloc_done - progress->graph_ready)
                               .count();
      timings.link_fixup = std::chrono::duration<double>(
                               progress->fixup_done - progress->alloc_done)
                               .count();
      timings.link_finalize =
          std::chrono::duration<double>(lookup_end - progress->fixup_done)
              .count();

      if (progress->emitted != Clock::time_point{}) {
        timings.finalize_perm = std::chrono::duration<double>(
                                    progress->emitted - progress->fixup_done)
                                    .count();
        timings.finalize_overhead =
            std::chrono::duration<double>(lookup_end - progress->emitted)
                .count();
      }

      timings.has_link_detail = true;
    }

    orc_stats.linker_backend = std::move(progress->linker_backend);
    orc_stats.object_count = progress->object_count;
    orc_stats.total_object_bytes = progress->total_object_bytes;
    orc_stats.relocation_count = progress->relocation_count;
    orc_stats.symbol_count = progress->symbol_count;
    orc_stats.section_count = progress->section_count;
    orc_stats.block_count = progress->block_count;
    orc_stats.finalize_segments = progress->finalize_segments;
    orc_stats.finalize_bytes = progress->finalize_bytes;
  }

  timings.complete = true;

  return CompileResult{
      .jit = std::move(*jit),
      .entry_fn = main_sym->toPtr<int()>(),
      .timings = timings,
      .orc_stats = std::move(orc_stats),
  };
}

}  // namespace

auto CompileJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    const JitCompileOptions& options)
    -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, runtime_path, options);
  if (!cr) return std::unexpected(cr.error());
  JitSession session;
  session.impl_ = std::make_unique<JitSession::Impl>();
  session.impl_->jit = std::move(cr->jit);
  session.impl_->entry_fn = cr->entry_fn;
  session.impl_->timings = cr->timings;
  session.impl_->orc_stats = std::move(cr->orc_stats);
  return session;
}

auto CompileJitInProcess(
    LoweringResult& result, const JitCompileOptions& options)
    -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, std::nullopt, options);
  if (!cr) return std::unexpected(cr.error());
  JitSession session;
  session.impl_ = std::make_unique<JitSession::Impl>();
  session.impl_->jit = std::move(cr->jit);
  session.impl_->entry_fn = cr->entry_fn;
  session.impl_->timings = cr->timings;
  session.impl_->orc_stats = std::move(cr->orc_stats);
  return session;
}

auto ExecuteWithOrcJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    const JitCompileOptions& options) -> std::expected<int, std::string> {
  auto session = CompileJit(result, runtime_path, options);
  if (!session) return std::unexpected(session.error());
  return session->Run();
}

auto ExecuteWithOrcJitInProcess(
    LoweringResult& result, const JitCompileOptions& options)
    -> std::expected<int, std::string> {
  auto session = CompileJitInProcess(result, options);
  if (!session) return std::unexpected(session.error());
  return session->Run();
}

}  // namespace lyra::lowering::mir_to_llvm
