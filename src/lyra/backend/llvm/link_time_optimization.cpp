#include "lyra/backend/llvm/link_time_optimization.hpp"

#include <cstddef>
#include <deque>
#include <filesystem>
#include <format>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <llvm/IR/DiagnosticInfo.h>
#include <llvm/IR/DiagnosticPrinter.h>
#include <llvm/LTO/Config.h>
#include <llvm/LTO/LTO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/IRObjectFile.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/SymbolicFile.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/Caching.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Threading.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/target_machine.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The symbol the system's start-up code calls, which no input here names.
constexpr std::string_view kEntryPoint = "main";

auto ReadFile(const std::filesystem::path& path)
    -> diag::Result<std::unique_ptr<llvm::MemoryBuffer>> {
  auto buffer = llvm::MemoryBuffer::getFile(path.string());
  if (!buffer) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read '{}' ({})", path.string(),
        buffer.getError().message());
  }
  return std::move(*buffer);
}

auto MessageOf(llvm::Error error) -> std::string {
  return llvm::toString(std::move(error));
}

// The names an object of machine code leaves undefined: what it needs some
// other input to define.
auto UndefinedIn(llvm::MemoryBufferRef object, std::vector<std::string>& names)
    -> diag::Result<void> {
  auto file = llvm::object::ObjectFile::createObjectFile(object);
  if (!file) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read '{}' as an object ({})",
        object.getBufferIdentifier().str(), MessageOf(file.takeError()));
  }
  for (const llvm::object::SymbolRef& symbol : (*file)->symbols()) {
    auto flags = symbol.getFlags();
    if (!flags) {
      llvm::consumeError(flags.takeError());
      continue;
    }
    if ((*flags & llvm::object::SymbolRef::SF_Undefined) == 0) {
      continue;
    }
    auto name = symbol.getName();
    if (!name) {
      llvm::consumeError(name.takeError());
      continue;
    }
    names.emplace_back(*name);
  }
  return {};
}

// Bitcode this compiler or the runtime's build wrote, which it can always read;
// failing to is a fault of this compiler's.
auto OpenBitcode(llvm::MemoryBufferRef bitcode)
    -> std::unique_ptr<llvm::lto::InputFile> {
  auto file = llvm::lto::InputFile::create(bitcode);
  if (!file) {
    throw InternalError(
        std::format(
            "link-time optimization: cannot read the bitcode of '{}': {}",
            bitcode.getBufferIdentifier().str(), MessageOf(file.takeError())));
  }
  return std::move(*file);
}

// The runtime library's members: those carrying bitcode, by the symbols they
// define, and what the ones carrying none leave undefined, since the host link
// takes those as machine code.
struct RuntimeMembers {
  std::unique_ptr<llvm::MemoryBuffer> archive_bytes;
  std::vector<std::unique_ptr<llvm::lto::InputFile>> bitcode;
  // Each member's name as the link knows it, which its bitcode's buffer refers
  // to, so growing the list must not move the ones already there.
  std::deque<std::string> names;
  std::unordered_map<std::string, std::size_t> defining;
  std::vector<std::string> undefined_in_machine_code;
};

auto ReadRuntime(const std::filesystem::path& library)
    -> diag::Result<RuntimeMembers> {
  RuntimeMembers members;
  auto bytes = ReadFile(library);
  if (!bytes) {
    return std::unexpected(std::move(bytes.error()));
  }
  members.archive_bytes = *std::move(bytes);
  auto archive =
      llvm::object::Archive::create(members.archive_bytes->getMemBufferRef());
  if (!archive) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read '{}' as an archive ({})",
        library.string(), MessageOf(archive.takeError()));
  }
  llvm::Error walk = llvm::Error::success();
  for (const llvm::object::Archive::Child& child : (*archive)->children(walk)) {
    auto member = child.getMemoryBufferRef();
    if (!member) {
      return diag::Fail(
          diag::DiagCode::kHostIoError, "cannot read a member of '{}' ({})",
          library.string(), MessageOf(member.takeError()));
    }
    auto bitcode = llvm::object::IRObjectFile::findBitcodeInMemBuffer(*member);
    if (!bitcode) {
      llvm::consumeError(bitcode.takeError());
      if (auto undefined =
              UndefinedIn(*member, members.undefined_in_machine_code);
          !undefined) {
        return std::unexpected(std::move(undefined.error()));
      }
      continue;
    }
    // A module is known to the link by its name, and two members of one
    // archive may share a file name, so the name says where in the archive
    // the member lies, as a linker names one.
    members.names.push_back(
        std::format(
            "{}({} at {})", library.string(),
            member->getBufferIdentifier().str(), child.getChildOffset()));
    std::unique_ptr<llvm::lto::InputFile> file = OpenBitcode(
        llvm::MemoryBufferRef(bitcode->getBuffer(), members.names.back()));
    const std::size_t index = members.bitcode.size();
    for (const llvm::lto::InputFile::Symbol& symbol : file->symbols()) {
      if (!symbol.isUndefined()) {
        members.defining.try_emplace(std::string(symbol.getName()), index);
      }
    }
    members.bitcode.push_back(std::move(file));
  }
  if (walk) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read the members of '{}' ({})",
        library.string(), MessageOf(std::move(walk)));
  }
  return members;
}

// Which runtime members a program takes: starting from what the program's own
// inputs leave undefined, each member defining a name still undefined, and
// then what that member leaves undefined in turn -- a linker's rule for an
// archive, so the program optimized here is the one the host link would make.
auto MembersTaken(
    const std::vector<std::unique_ptr<llvm::lto::InputFile>>& modules,
    const RuntimeMembers& runtime, std::span<const std::string> roots)
    -> std::vector<std::size_t> {
  std::unordered_set<std::string> defined;
  std::vector<std::string> wanted(roots.begin(), roots.end());
  auto take_symbols = [&](const llvm::lto::InputFile& file) {
    for (const llvm::lto::InputFile::Symbol& symbol : file.symbols()) {
      if (symbol.isUndefined()) {
        wanted.emplace_back(symbol.getName());
      } else {
        defined.emplace(symbol.getName());
      }
    }
  };
  for (const auto& module : modules) {
    take_symbols(*module);
  }
  std::vector<bool> taken(runtime.bitcode.size(), false);
  std::vector<std::size_t> order;
  while (!wanted.empty()) {
    const std::string name = std::move(wanted.back());
    wanted.pop_back();
    if (defined.contains(name)) {
      continue;
    }
    const auto member = runtime.defining.find(name);
    if (member == runtime.defining.end() || taken[member->second]) {
      continue;
    }
    taken[member->second] = true;
    order.push_back(member->second);
    take_symbols(*runtime.bitcode[member->second]);
  }
  return order;
}

// How each symbol of `file` resolves, as a linker resolves it: the first
// definition met prevails, and a definition anything outside the bitcode
// names stays visible to it.
auto Resolve(
    const llvm::lto::InputFile& file, std::unordered_set<std::string>& defined,
    const std::unordered_set<std::string>& named_outside)
    -> std::vector<llvm::lto::SymbolResolution> {
  std::vector<llvm::lto::SymbolResolution> resolutions;
  for (const llvm::lto::InputFile::Symbol& symbol : file.symbols()) {
    llvm::lto::SymbolResolution resolution;
    const std::string name(symbol.getName());
    if (!symbol.isUndefined() && defined.insert(name).second) {
      resolution.Prevailing = 1;
      resolution.FinalDefinitionInLinkageUnit = 1;
    }
    resolution.VisibleToRegularObj = named_outside.contains(name) ? 1 : 0;
    resolutions.push_back(resolution);
  }
  return resolutions;
}

// Where task `task`'s object is written.
auto ObjectPath(const std::filesystem::path& dir, unsigned task)
    -> std::filesystem::path {
  return dir / std::format("{}.o", task);
}

}  // namespace

auto OptimizeAtLink(
    const LinkTimeInputs& inputs, llvm::OptimizationLevel level,
    const std::filesystem::path& dir, std::size_t width,
    const std::optional<std::filesystem::path>& cache)
    -> diag::Result<std::vector<std::filesystem::path>> {
  const CompileTarget& target = ProcessTarget();

  std::vector<std::unique_ptr<llvm::MemoryBuffer>> module_bytes;
  std::vector<std::unique_ptr<llvm::lto::InputFile>> modules;
  for (const std::filesystem::path& path : inputs.modules) {
    auto bytes = ReadFile(path);
    if (!bytes) {
      return std::unexpected(std::move(bytes.error()));
    }
    module_bytes.push_back(*std::move(bytes));
    modules.push_back(OpenBitcode(module_bytes.back()->getMemBufferRef()));
  }

  auto runtime = ReadRuntime(inputs.runtime_library);
  if (!runtime) {
    return std::unexpected(std::move(runtime.error()));
  }

  // What the machine code of the program names: the start-up code's entry,
  // what each object handed in as machine code leaves undefined, and what the
  // runtime's members without bitcode do.
  std::vector<std::string> named_outside_list{std::string(kEntryPoint)};
  for (const std::filesystem::path& path : inputs.machine_code) {
    auto bytes = ReadFile(path);
    if (!bytes) {
      return std::unexpected(std::move(bytes.error()));
    }
    if (auto undefined =
            UndefinedIn((*bytes)->getMemBufferRef(), named_outside_list);
        !undefined) {
      return std::unexpected(std::move(undefined.error()));
    }
  }
  named_outside_list.insert(
      named_outside_list.end(), runtime->undefined_in_machine_code.begin(),
      runtime->undefined_in_machine_code.end());
  const std::unordered_set<std::string> named_outside(
      named_outside_list.begin(), named_outside_list.end());

  const std::vector<std::size_t> taken =
      MembersTaken(modules, *runtime, named_outside_list);

  std::mutex diagnostics_mutex;
  std::vector<std::string> errors;
  // A file this link could not write is the machine's failure rather than
  // the compiler's, and is reported as one.
  std::vector<std::string> write_failures;
  auto failed_to_write = [&](const std::string& path, std::error_code ec) {
    const std::scoped_lock lock(diagnostics_mutex);
    write_failures.push_back(
        std::format("cannot write '{}' ({})", path, ec.message()));
  };
  llvm::lto::Config config;
  config.CPU = std::string(kTargetCpu);
  config.RelocModel = kRelocation;
  config.OptLevel = static_cast<unsigned>(level);
  config.CGOptLevel = CodeGenLevelOf(level);
  config.DefaultTriple = target.triple.str();
  config.DiagHandler = [&](const llvm::DiagnosticInfo& info) {
    if (info.getSeverity() != llvm::DS_Error) {
      return;
    }
    std::string text;
    llvm::raw_string_ostream out(text);
    llvm::DiagnosticPrinterRawOStream printer(out);
    info.print(printer);
    const std::scoped_lock lock(diagnostics_mutex);
    errors.push_back(std::move(text));
  };
  llvm::lto::LTO lto(
      std::move(config), llvm::lto::createInProcessThinBackend(
                             llvm::heavyweight_hardware_concurrency(
                                 static_cast<unsigned>(width))));

  std::unordered_set<std::string> defined;
  auto add = [&](std::unique_ptr<llvm::lto::InputFile> file) {
    std::vector<llvm::lto::SymbolResolution> resolutions =
        Resolve(*file, defined, named_outside);
    if (llvm::Error added = lto.add(std::move(file), resolutions)) {
      throw InternalError(
          std::format(
              "link-time optimization: cannot add a module: {}",
              MessageOf(std::move(added))));
    }
  };
  for (auto& module : modules) {
    add(std::move(module));
  }
  for (const std::size_t member : taken) {
    add(std::move(runtime->bitcode[member]));
  }

  std::error_code made;
  std::filesystem::create_directories(dir, made);
  if (made) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot create '{}' ({})", dir.string(),
        made.message());
  }
  const unsigned tasks = lto.getMaxTasks();
  std::vector<bool> written(tasks, false);
  std::mutex written_mutex;
  auto mark = [&](unsigned task) {
    const std::scoped_lock lock(written_mutex);
    written[task] = true;
  };
  llvm::AddStreamFn add_stream = [&](unsigned task, const llvm::Twine&)
      -> llvm::Expected<std::unique_ptr<llvm::CachedFileStream>> {
    const std::string path = ObjectPath(dir, task).string();
    std::error_code opened;
    auto out = std::make_unique<llvm::raw_fd_ostream>(
        path, opened, llvm::sys::fs::OF_None);
    if (opened) {
      failed_to_write(path, opened);
      return llvm::errorCodeToError(opened);
    }
    mark(task);
    return std::make_unique<llvm::CachedFileStream>(std::move(out), path);
  };

  llvm::FileCache file_cache;
  if (cache) {
    auto opened = llvm::localCache(
        "ThinLTO", "Thin", cache->string(),
        [&](std::size_t task, const llvm::Twine&,
            std::unique_ptr<llvm::MemoryBuffer> object) {
          const std::string path =
              ObjectPath(dir, static_cast<unsigned>(task)).string();
          std::error_code opened_out;
          llvm::raw_fd_ostream out(path, opened_out, llvm::sys::fs::OF_None);
          if (opened_out) {
            failed_to_write(path, opened_out);
            return;
          }
          out << object->getBuffer();
          mark(static_cast<unsigned>(task));
          // The store trims what went unused by when it was last written, and
          // this cache marks a hit only as read, so a hit is marked used here.
          std::error_code touched;
          std::filesystem::last_write_time(
              object->getBufferIdentifier().str(),
              std::filesystem::file_time_type::clock::now(), touched);
        });
    if (!opened) {
      return diag::Fail(
          diag::DiagCode::kHostIoError, "cannot open the cache at '{}' ({})",
          cache->string(), MessageOf(opened.takeError()));
    }
    file_cache = *std::move(opened);
  }
  llvm::Error ran = lto.run(add_stream, file_cache);
  if (!write_failures.empty()) {
    llvm::consumeError(std::move(ran));
    return diag::Fail(
        diag::DiagCode::kHostIoError, "{}", write_failures.front());
  }
  if (ran) {
    errors.push_back(MessageOf(std::move(ran)));
  }
  if (!errors.empty()) {
    std::string joined;
    for (const std::string& error : errors) {
      joined += error;
      joined += '\n';
    }
    throw InternalError(
        std::format("link-time optimization failed:\n{}", joined));
  }

  std::vector<std::filesystem::path> objects;
  for (unsigned task = 0; task < tasks; ++task) {
    if (written[task]) {
      objects.push_back(ObjectPath(dir, task));
    }
  }
  return objects;
}

}  // namespace lyra::backend::llvm_backend
