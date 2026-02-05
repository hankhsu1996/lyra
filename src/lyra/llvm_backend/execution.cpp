#include "lyra/llvm_backend/execution.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <mutex>
#include <optional>
#include <string>
#include <utility>

#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
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
};

JitSession::JitSession() = default;
JitSession::~JitSession() = default;
JitSession::JitSession(JitSession&&) noexcept = default;
JitSession& JitSession::operator=(JitSession&&) noexcept = default;

auto JitSession::Run() -> int {
  return impl_->entry_fn();
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
};

auto CompileJitImpl(
    LoweringResult& result,
    const std::optional<std::filesystem::path>& runtime_path,
    OptLevel opt_level) -> std::expected<CompileResult, std::string> {
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

  // Transfer module ownership to JIT
  llvm::orc::ThreadSafeContext tsc(std::move(result.context));
  auto tsm = llvm::orc::ThreadSafeModule(std::move(result.module), tsc);
  if (auto err = (*jit)->addIRModule(std::move(tsm))) {
    return std::unexpected(
        std::format(
            "failed to add module: {}", llvm::toString(std::move(err))));
  }

  // Symbol lookup triggers JIT compilation of the module
  auto main_sym = (*jit)->lookup("main");
  if (!main_sym) {
    return std::unexpected(
        std::format(
            "symbol 'main' not found: {}",
            llvm::toString(main_sym.takeError())));
  }

  return CompileResult{
      .jit = std::move(*jit),
      .entry_fn = main_sym->toPtr<int()>(),
  };
}

}  // namespace

auto CompileJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    OptLevel opt_level) -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, runtime_path, opt_level);
  if (!cr) return std::unexpected(cr.error());
  JitSession session;
  session.impl_ = std::make_unique<JitSession::Impl>();
  session.impl_->jit = std::move(cr->jit);
  session.impl_->entry_fn = cr->entry_fn;
  return session;
}

auto CompileJitInProcess(LoweringResult& result, OptLevel opt_level)
    -> std::expected<JitSession, std::string> {
  auto cr = CompileJitImpl(result, std::nullopt, opt_level);
  if (!cr) return std::unexpected(cr.error());
  JitSession session;
  session.impl_ = std::make_unique<JitSession::Impl>();
  session.impl_->jit = std::move(cr->jit);
  session.impl_->entry_fn = cr->entry_fn;
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
