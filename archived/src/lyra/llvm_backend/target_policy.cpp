#include "lyra/llvm_backend/target_policy.hpp"

#include <cstdlib>
#include <format>
#include <memory>
#include <mutex>
#include <string>

#include <llvm/ADT/StringMap.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

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

auto ResolveNativeConfig() -> TargetConfig {
  std::string cpu = llvm::sys::getHostCPUName().str();

  llvm::StringMap<bool> feature_map;
  llvm::sys::getHostCPUFeatures(feature_map);
  llvm::SubtargetFeatures subtarget_features;
  for (const auto& kv : feature_map) {
    if (kv.getValue()) {
      subtarget_features.AddFeature(kv.getKey().str());
    }
  }

  return TargetConfig{
      .triple = llvm::sys::getDefaultTargetTriple(),
      .cpu = std::move(cpu),
      .features = subtarget_features.getString(),
  };
}

auto ResolvePortableConfig() -> TargetConfig {
  // x86-64-v3: AVX2 + FMA + BMI1/BMI2 + F16C + MOVBE + LZCNT.
  // Covers all GitHub Actions runners (AMD EPYC, Intel Xeon Scalable)
  // and all x86-64 CPUs from ~2015 onward. Does NOT include AVX-512.
  return TargetConfig{
      .triple = llvm::sys::getDefaultTargetTriple(),
      .cpu = "x86-64-v3",
      .features = {},
  };
}

auto CreateTargetMachineFromConfig(
    const TargetConfig& config, OptLevel opt_level,
    std::optional<llvm::Reloc::Model> reloc)
    -> std::unique_ptr<llvm::TargetMachine> {
  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(config.triple, error);
  if (target == nullptr) {
    throw common::InternalError(
        "CreateTargetMachineFromConfig",
        std::format(
            "failed to lookup target for '{}': {}", config.triple, error));
  }

  std::unique_ptr<llvm::TargetMachine> tm(target->createTargetMachine(
      config.triple, config.cpu, config.features, llvm::TargetOptions(), reloc,
      std::nullopt, ToCodeGenOpt(opt_level)));
  if (tm == nullptr) {
    throw common::InternalError(
        "CreateTargetMachineFromConfig",
        std::format("failed to create TargetMachine for '{}'", config.triple));
  }

  return tm;
}

}  // namespace

auto ResolveTargetConfig(IsaPolicy policy) -> TargetConfig {
  switch (policy) {
    case IsaPolicy::kPortable:
      return ResolvePortableConfig();
    case IsaPolicy::kNative:
      return ResolveNativeConfig();
  }
  throw common::InternalError("ResolveTargetConfig", "unknown IsaPolicy");
}

auto CreateTargetMachine(IsaPolicy policy, OptLevel opt_level)
    -> std::unique_ptr<llvm::TargetMachine> {
  InitializeLlvmTargets();

  auto config = ResolveTargetConfig(policy);

  // Diagnostic: log exact target selection when LYRA_DUMP_TARGET is set.
  if (std::getenv("LYRA_DUMP_TARGET") != nullptr) {
    auto msg = std::format(
        "=== LYRA TARGET SELECTION ===\n"
        "  policy:   {}\n"
        "  triple:   {}\n"
        "  cpu:      {}\n"
        "  features: {}\n"
        "  opt:      {}\n"
        "  reloc:    PIC\n"
        "=== END TARGET SELECTION ===\n",
        policy == IsaPolicy::kPortable ? "portable" : "native", config.triple,
        config.cpu, config.features, opt_level == OptLevel::kO2 ? "O2" : "O0");
    fputs(msg.c_str(), stderr);
  }

  return CreateTargetMachineFromConfig(config, opt_level, llvm::Reloc::PIC_);
}

void SetModuleDataLayout(llvm::Module& module) {
  InitializeLlvmTargets();

  // Use portable config for DataLayout. The DataLayout determines struct
  // alignment and type sizes, which must be consistent between lowering
  // and codegen. x86-64-v3 and native produce identical DataLayout on
  // x86-64 (alignment rules depend on the ABI, not the ISA level).
  auto config = ResolvePortableConfig();
  auto tm = CreateTargetMachineFromConfig(config, OptLevel::kO0, std::nullopt);

  module.setTargetTriple(config.triple);
  module.setDataLayout(tm->createDataLayout());
}

}  // namespace lyra::lowering::mir_to_llvm
