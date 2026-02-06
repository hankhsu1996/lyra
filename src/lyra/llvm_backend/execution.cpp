#include "lyra/llvm_backend/execution.hpp"

#include <atomic>
#include <chrono>
#include <cstdio>
#include <expected>
#include <filesystem>
#include <format>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <utility>

#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

#include "lyra/common/internal_error.hpp"

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
  int relocation_count = 0;
  int symbol_count = 0;
  int section_count = 0;
  int block_count = 0;
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
        [this](llvm::jitlink::LinkGraph&) -> llvm::Error {
          std::lock_guard lock(progress_->mutex);
          progress_->fixup_done = Clock::now();
          return llvm::Error::success();
        });
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

// Prints JIT link progress to stderr every 5 seconds until stopped.
// Uses short sleep intervals so join() returns promptly after stop is set.
void PrintLinkProgress(LinkProgress& progress) {
  using namespace std::chrono_literals;
  constexpr int kIntervalMs = 5000;
  constexpr int kPollMs = 100;
  int ms_since_print = 0;
  while (!progress.stop.load(std::memory_order_relaxed)) {
    std::this_thread::sleep_for(std::chrono::milliseconds(kPollMs));
    if (progress.stop.load(std::memory_order_relaxed)) break;
    ms_since_print += kPollMs;
    if (ms_since_print < kIntervalMs) continue;
    ms_since_print = 0;

    std::lock_guard lock(progress.mutex);
    auto now = Clock::now();
    double elapsed =
        std::chrono::duration<double>(now - progress.lookup_start).count();

    // Determine current phase from timestamps.
    const char* phase = "codegen";
    if (progress.codegen_end != Clock::time_point{}) {
      phase = "link_graph";
      if (progress.graph_ready != Clock::time_point{}) {
        phase = "link_alloc";
        if (progress.alloc_done != Clock::time_point{}) {
          phase = "link_fixup";
          if (progress.fixup_done != Clock::time_point{}) {
            phase = "link_finalize";
          }
        }
      }
    }

    auto msg =
        std::format("[lyra][jit][progress] {:.1f}s phase={}", elapsed, phase);

    if (progress.object_count > 0) {
      msg += std::format(
          " objects={} obj_bytes={}", progress.object_count,
          progress.total_object_bytes);
    }

    if (progress.relocation_count > 0) {
      msg += std::format(
          " relocs={} syms={} sections={} blocks={}", progress.relocation_count,
          progress.symbol_count, progress.section_count, progress.block_count);
    }

    // Completed sub-phase timings.
    if (progress.codegen_end != Clock::time_point{}) {
      msg += std::format(
          " codegen={:.3f}s", std::chrono::duration<double>(
                                  progress.codegen_end - progress.lookup_start)
                                  .count());
    }
    if (progress.graph_ready != Clock::time_point{} &&
        progress.codegen_end != Clock::time_point{}) {
      msg += std::format(
          " link_graph={:.3f}s",
          std::chrono::duration<double>(
              progress.graph_ready - progress.codegen_end)
              .count());
    }
    if (progress.alloc_done != Clock::time_point{} &&
        progress.graph_ready != Clock::time_point{}) {
      msg += std::format(
          " link_alloc={:.3f}s", std::chrono::duration<double>(
                                     progress.alloc_done - progress.graph_ready)
                                     .count());
    }
    if (progress.fixup_done != Clock::time_point{} &&
        progress.alloc_done != Clock::time_point{}) {
      msg += std::format(
          " link_fixup={:.3f}s", std::chrono::duration<double>(
                                     progress.fixup_done - progress.alloc_done)
                                     .count());
    }

    msg += "\n";
    fputs(msg.c_str(), stderr);
    fflush(stderr);
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
    OptLevel opt_level, bool emit_progress)
    -> std::expected<CompileResult, std::string> {
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
  jtmb->setCodeGenOptLevel(ToCodeGenOpt(opt_level));
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

  // Detect linker backend and install profiling plugin.
  LinkProgress progress;
  if (auto* oll = llvm::dyn_cast<llvm::orc::ObjectLinkingLayer>(
          &(*jit)->getObjLinkingLayer())) {
    progress.linker_backend = "JITLink";
    oll->addPlugin(std::make_unique<ProfilingPlugin>(&progress));
  } else {
    progress.linker_backend = "RTDyld";
  }

  // Install object transform to measure codegen->link boundary.
  // The transform fires after IR compilation produces an object buffer
  // but before JITLink/RTDyld processes it.
  (*jit)->getObjTransformLayer().setTransform(
      [&progress](std::unique_ptr<llvm::MemoryBuffer> buf)
          -> llvm::Expected<std::unique_ptr<llvm::MemoryBuffer>> {
        std::lock_guard lock(progress.mutex);
        progress.object_count++;
        progress.total_object_bytes +=
            static_cast<int64_t>(buf->getBufferSize());
        progress.codegen_end = Clock::now();
        return buf;
      });

  // Sub-phase 3: add_ir
  auto t2 = Clock::now();
  llvm::orc::ThreadSafeContext tsc(std::move(result.context));
  auto tsm = llvm::orc::ThreadSafeModule(std::move(result.module), tsc);
  if (auto err = (*jit)->addIRModule(std::move(tsm))) {
    return std::unexpected(
        std::format(
            "failed to add module: {}", llvm::toString(std::move(err))));
  }
  timings.add_ir = ElapsedSeconds(t2);

  // Sub-phase 4: lookup_main (triggers full JIT compilation + linking)
  progress.lookup_start = Clock::now();

  std::optional<std::thread> watchdog;
  if (emit_progress) {
    watchdog.emplace([&progress] { PrintLinkProgress(progress); });
  }

  auto main_sym = (*jit)->lookup("main");

  progress.stop.store(true, std::memory_order_relaxed);
  if (watchdog) watchdog->join();

  if (!main_sym) {
    return std::unexpected(
        std::format(
            "symbol 'main' not found: {}",
            llvm::toString(main_sym.takeError())));
  }

  // Compute timings from progress (single-threaded after join, no lock).
  auto lookup_end = Clock::now();
  timings.lookup_main =
      std::chrono::duration<double>(lookup_end - progress.lookup_start).count();
  if (progress.object_count > 0) {
    timings.codegen = std::chrono::duration<double>(
                          progress.codegen_end - progress.lookup_start)
                          .count();
    timings.linking = timings.lookup_main - timings.codegen;
  }

  if (progress.fixup_done != Clock::time_point{}) {
    timings.link_graph = std::chrono::duration<double>(
                             progress.graph_ready - progress.codegen_end)
                             .count();
    timings.link_alloc = std::chrono::duration<double>(
                             progress.alloc_done - progress.graph_ready)
                             .count();
    timings.link_fixup =
        std::chrono::duration<double>(progress.fixup_done - progress.alloc_done)
            .count();
    timings.link_finalize =
        std::chrono::duration<double>(lookup_end - progress.fixup_done).count();
    timings.has_link_detail = true;
  }
  timings.complete = true;

  JitOrcStats orc_stats;
  orc_stats.linker_backend = std::move(progress.linker_backend);
  orc_stats.object_count = progress.object_count;
  orc_stats.total_object_bytes = progress.total_object_bytes;
  orc_stats.relocation_count = progress.relocation_count;
  orc_stats.symbol_count = progress.symbol_count;
  orc_stats.section_count = progress.section_count;
  orc_stats.block_count = progress.block_count;

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
    OptLevel opt_level, bool emit_progress)
    -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, runtime_path, opt_level, emit_progress);
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
    LoweringResult& result, OptLevel opt_level, bool emit_progress)
    -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, std::nullopt, opt_level, emit_progress);
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
    OptLevel opt_level) -> std::expected<int, std::string> {
  auto session = CompileJit(result, runtime_path, opt_level);
  if (!session) return std::unexpected(session.error());
  return session->Run();
}

auto ExecuteWithOrcJitInProcess(LoweringResult& result, OptLevel opt_level)
    -> std::expected<int, std::string> {
  auto session = CompileJitInProcess(result, opt_level);
  if (!session) return std::unexpected(session.error());
  return session->Run();
}

}  // namespace lyra::lowering::mir_to_llvm
