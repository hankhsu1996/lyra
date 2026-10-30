#include "lyra/driver/dpi_boundary.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <iterator>
#include <optional>
#include <span>
#include <string>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/BinaryFormat/ELF.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/Binary.h>
#include <llvm/Object/ELF.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/MemoryBuffer.h>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/sanitizer.hpp"

namespace lyra::driver {

namespace {

auto UnrecognizedInput(const std::filesystem::path& path)
    -> diag::Result<DpiLinkInput> {
  return diag::Fail(
      diag::DiagCode::kHostInvalidCliArgs,
      "DPI-C link input '{}' is neither a C or C++ source nor an ELF "
      "object, archive or shared library",
      path.string());
}

// The soname a shared library declares in its dynamic section, which is the
// name the dynamic loader looks for it by. The section's strings are the table
// its header links it to.
template <class ELFT>
auto SonameIn(
    const std::filesystem::path& path, const llvm::object::ELFFile<ELFT>& elf)
    -> diag::Result<std::optional<std::string>> {
  const auto unreadable = [&](llvm::Error error) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "cannot read the dynamic section of '{}' ({})", path.string(),
        llvm::toString(std::move(error)));
  };
  auto sections = elf.sections();
  if (!sections) {
    return unreadable(sections.takeError());
  }
  const auto dynamic = std::ranges::find(
      *sections, llvm::ELF::SHT_DYNAMIC,
      [](const auto& section) { return section.sh_type; });
  if (dynamic == sections->end()) {
    return std::nullopt;
  }
  auto strings = elf.getLinkAsStrtab(*dynamic);
  if (!strings) {
    return unreadable(strings.takeError());
  }
  auto entries = elf.dynamicEntries();
  if (!entries) {
    return unreadable(entries.takeError());
  }
  const auto soname = std::ranges::find(
      *entries, llvm::ELF::DT_SONAME,
      [](const auto& entry) { return entry.getTag(); });
  if (soname == entries->end()) {
    return std::nullopt;
  }
  if (soname->getVal() >= strings->size()) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "'{}' names a soname outside its dynamic string table", path.string());
  }
  return strings->drop_front(soname->getVal())
      .take_until([](char c) { return c == '\0'; })
      .str();
}

auto SonameOf(
    const std::filesystem::path& path,
    const llvm::object::ELFObjectFileBase& object)
    -> diag::Result<std::optional<std::string>> {
  using llvm::object::ELF32BEObjectFile;
  using llvm::object::ELF32LEObjectFile;
  using llvm::object::ELF64BEObjectFile;
  using llvm::object::ELF64LEObjectFile;
  if (const auto* elf = llvm::dyn_cast<ELF64LEObjectFile>(&object)) {
    return SonameIn(path, elf->getELFFile());
  }
  if (const auto* elf = llvm::dyn_cast<ELF64BEObjectFile>(&object)) {
    return SonameIn(path, elf->getELFFile());
  }
  if (const auto* elf = llvm::dyn_cast<ELF32LEObjectFile>(&object)) {
    return SonameIn(path, elf->getELFFile());
  }
  return SonameIn(path, llvm::cast<ELF32BEObjectFile>(object).getELFFile());
}

// What a file handed over compiled is, read from its contents, so a file is
// taken for what it holds whatever it is called -- a versioned shared library
// like `libfoo.so.1` included. Only what the link reads as itself is taken:
// whether it suits the machine and the linker is the linker's to say, but a
// file the linker would read as a script, or a thin archive whose members lie
// elsewhere, brings files into the program that neither the kept program nor
// an emitted project's copies would know of.
auto ClassifyCompiled(const std::filesystem::path& path)
    -> diag::Result<DpiLinkInput> {
  auto buffer = llvm::MemoryBuffer::getFile(path.string(), false, false);
  if (!buffer) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "DPI-C link input '{}' is not a readable file", path.string());
  }
  auto binary = llvm::object::createBinary((*buffer)->getMemBufferRef());
  if (!binary) {
    llvm::consumeError(binary.takeError());
    return UnrecognizedInput(path);
  }
  if (const auto* archive =
          llvm::dyn_cast<llvm::object::Archive>(binary->get())) {
    if (archive->isThin()) {
      return std::unexpected(
          diag::Make(
              diag::DiagCode::kHostInvalidCliArgs,
              "DPI-C link input '{}' is a thin archive, which names its "
              "members rather than holding them",
              path.string())
              .WithNote("give an archive that holds its members"));
    }
    return DpiCompiledCode{.path = path};
  }
  const auto* object =
      llvm::dyn_cast<llvm::object::ELFObjectFileBase>(binary->get());
  if (object == nullptr) {
    return UnrecognizedInput(path);
  }
  if (object->getEType() == llvm::ELF::ET_REL) {
    return DpiCompiledCode{.path = path};
  }
  if (object->getEType() == llvm::ELF::ET_DYN) {
    auto soname = SonameOf(path, *object);
    if (!soname) {
      return std::unexpected(std::move(soname.error()));
    }
    // The program finds it here whenever it runs, wherever it is run from.
    std::error_code ec;
    std::filesystem::path loaded_from = std::filesystem::absolute(path, ec);
    if (ec) {
      return diag::Fail(
          diag::DiagCode::kHostIoError, "cannot tell where '{}' lies ({})",
          path.string(), ec.message());
    }
    return DpiSharedLibrary{
        .path = std::move(loaded_from), .soname = *std::move(soname)};
  }
  return UnrecognizedInput(path);
}

// A source is told by its extension, as a compiler driver tells one; anything
// else by its contents.
auto Classify(const std::filesystem::path& path) -> diag::Result<DpiLinkInput> {
  const std::string ext = path.extension().string();
  if (ext == ".c") {
    return DpiSource{.path = path, .compile_as_c = true};
  }
  if (ext == ".cpp" || ext == ".cc" || ext == ".cxx") {
    return DpiSource{.path = path, .compile_as_c = false};
  }
  return ClassifyCompiled(path);
}

}  // namespace

auto DpiInputPath(const DpiLinkInput& input) -> const std::filesystem::path& {
  return std::visit(
      [](const auto& kind) -> const std::filesystem::path& {
        return kind.path;
      },
      input);
}

auto GatheredNameOf(const DpiLinkInput& input) -> std::string {
  return std::visit(
      Overloaded{
          [](const DpiSource& source) {
            return source.path.filename().string();
          },
          [](const DpiCompiledCode& code) {
            return code.path.filename().string();
          },
          [](const DpiSharedLibrary& library) {
            return library.soname ? std::filesystem::path(*library.soname)
                                        .filename()
                                        .string()
                                  : library.path.filename().string();
          }},
      input);
}

auto ValidateDpiLinkInputs(std::span<const std::string> inputs)
    -> diag::Result<std::vector<DpiLinkInput>> {
  std::vector<DpiLinkInput> classified;
  classified.reserve(inputs.size());
  for (const std::string& input : inputs) {
    const std::filesystem::path path{input};
    std::error_code ec;
    if (!std::filesystem::is_regular_file(path, ec)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "DPI-C link input '{}' is not a readable file", input);
    }
    // LRM J.4: any library is loaded once, and a file reached by two names --
    // a link, a second spelling of its path -- is the same file.
    const auto same_file = [&](const DpiLinkInput& seen) {
      std::error_code unequal;
      return std::filesystem::equivalent(DpiInputPath(seen), path, unequal);
    };
    if (std::ranges::any_of(classified, same_file)) {
      continue;
    }
    auto kind = Classify(path);
    if (!kind) {
      return std::unexpected(std::move(kind.error()));
    }
    // Every path that consumes these inputs names what it derives from them
    // by this name -- an object, a copy, the file the dynamic loader looks
    // for -- so two inputs sharing one would stand for each other rather than
    // both reach the program. Reject the ambiguity here instead of letting
    // each consumer discover it -- or, worse, not discover it.
    const std::string name = GatheredNameOf(*kind);
    const auto same_name = [&](const DpiLinkInput& seen) {
      return GatheredNameOf(seen) == name;
    };
    if (std::ranges::any_of(classified, same_name)) {
      return std::unexpected(
          diag::Make(
              diag::DiagCode::kHostInvalidCliArgs,
              "two DPI-C link inputs are both known as '{}'", name)
              .WithNote(
                  "give each input a file name of its own, and each shared "
                  "library a soname of its own"));
    }
    classified.push_back(*std::move(kind));
  }
  return classified;
}

auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const dpi::AbiFragment> fragments,
    const std::filesystem::path& dir) -> diag::Result<void> {
  for (const dpi::AbiFragment& fragment : fragments) {
    if (auto r = WriteFile(dir / fragment.relpath, fragment.text); !r) {
      return r;
    }
  }
  if (auto r = WriteFile(dir / kDpiAbiHeader, dpi::RenderAbiHeader(fragments));
      !r) {
    return r;
  }
  return CopyFile(runtime.svdpi_header, dir / kSvdpiHeader);
}

auto ForeignCompileFlags(
    const DpiSource& source, OptimizationLevel optimization,
    support::Sanitizer sanitizer) -> std::vector<std::string> {
  std::vector<std::string> flags =
      source.compile_as_c ? std::vector<std::string>{"-x", "c"}
                          : std::vector<std::string>{
                                std::string(kCxxStandardFlag), "-x", "c++"};
  flags.emplace_back(OptimizationFlag(optimization));
  std::ranges::move(
      SanitizerCompileFlags(sanitizer), std::back_inserter(flags));
  return flags;
}

auto CompileDpiLink(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    OptimizationLevel optimization, support::Sanitizer sanitizer,
    std::size_t width, const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::vector<DpiLinked>> {
  std::error_code created;
  std::filesystem::create_directories(work_dir, created);
  if (created) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot create '{}' ({})",
        work_dir.string(), created.message());
  }
  std::vector<DpiLinked> linked;
  linked.reserve(inputs.size());
  std::vector<ProcessRequest> requests;
  std::vector<std::string> names;
  std::vector<const DpiSource*> sources;
  for (const DpiLinkInput& input : inputs) {
    linked.push_back(
        std::visit(
            Overloaded{
                [&](const DpiSource& source) -> DpiLinked {
                  // One compilation per source, because the language each is
                  // compiled as is its own and a driver invocation carries one
                  // output path. The object's name extends the source's whole
                  // file name, which is the thing two inputs are already
                  // required to differ in; taking the stem instead would let
                  // one source and another of a different language overwrite
                  // each other.
                  const std::filesystem::path object =
                      work_dir / (GatheredNameOf(input) + ".o");
                  std::vector<std::string> args =
                      ForeignCompileFlags(source, optimization, sanitizer);
                  args.insert(
                      args.end(), {"-c", source.path.string(), "-I",
                                   header_dir.string(), "-o", object.string()});
                  requests.push_back(
                      ProcessRequest{.exe = cxx, .args = std::move(args)});
                  names.push_back(source.path.filename().string());
                  sources.push_back(&source);
                  return DpiCompiledCode{.path = object};
                },
                [](const DpiCompiledCode& code) -> DpiLinked { return code; },
                [](const DpiSharedLibrary& library) -> DpiLinked {
                  return library;
                }},
            input));
  }

  const status::Phase phase("Compiling DPI sources");
  auto compiled = RunProcessesCaptured(requests, width, names);
  if (!compiled) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(compiled.error()));
  }
  // Every source that failed is reported, the first as the error and each
  // later one as notes of it.
  std::optional<diag::Diagnostic> failures;
  for (std::size_t i = 0; i < sources.size(); ++i) {
    const ProcessResult& result = (*compiled)[i];
    if (result.exit_code == 0) {
      continue;
    }
    const std::string source = sources[i]->path.string();
    if (!failures) {
      failures = diag::Make(
          diag::DiagCode::kHostBuildFailed,
          "compiling the DPI-C link input '{}' failed", source);
    } else {
      failures = std::move(*failures).WithNote(
          "compiling the DPI-C link input '{}' failed", source);
    }
    failures = std::move(*failures).WithNote("{}", result.stderr_text);
  }
  if (failures) {
    return std::unexpected(*std::move(failures));
  }
  return linked;
}

auto DpiLinkArguments(std::span<const DpiLinked> linked)
    -> std::vector<std::string> {
  std::vector<std::string> arguments;
  for (const DpiLinked& file : linked) {
    std::visit(
        Overloaded{
            [&](const DpiCompiledCode& code) {
              arguments.push_back(code.path.string());
            },
            // Handed over by its path, so a library declaring no soname is
            // recorded by that path and no search can find another file of
            // its name first.
            [&](const DpiSharedLibrary& library) {
              arguments.insert(
                  arguments.end(), {"-Wl,--push-state,--no-as-needed",
                                    library.path.string(), "-Wl,--pop-state"});
              // Passed whole, since a driver splits a -Wl, list at its
              // commas and a directory may hold one.
              if (library.soname) {
                arguments.insert(
                    arguments.end(), {"-Xlinker", "-rpath", "-Xlinker",
                                      library.path.parent_path().string()});
              }
            }},
        file);
  }
  return arguments;
}

}  // namespace lyra::driver
