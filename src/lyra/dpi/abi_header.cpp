#include "lyra/dpi/abi_header.hpp"

#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::dpi {

namespace {

// The C spelling of a type that crosses the DPI-C boundary (LRM 35.5.6, Annex
// H) -- the C target's type mapping, the peer of the emitted backend's. The
// set is closed: a foreign signature names only machine scalars, a borrowed
// pointer to one, a canonical vector chunk, or an open-array handle, so
// anything else reaching here is a boundary the lowering should have rejected.
auto RenderTypeAsC(const mir::CompilationUnit& unit, mir::TypeId id)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::VoidType&) -> std::string { return "void"; },
          [](const mir::MachineIntType& m) -> std::string {
            return std::format(
                "{}int{}_t",
                m.signedness == mir::Signedness::kUnsigned ? "u" : "",
                m.bit_width);
          },
          [](const mir::MachineFloatType& f) -> std::string {
            if (f.bit_width == 32) {
              return "float";
            }
            if (f.bit_width == 64) {
              return "double";
            }
            throw InternalError(
                "RenderTypeAsC: no C floating type of this width crosses the "
                "DPI-C boundary");
          },
          [](const mir::MachineCStringType&) -> std::string {
            return "const char*";
          },
          [&](const mir::PointerType& p) -> std::string {
            return std::format(
                "{}{}*",
                p.mutability == mir::Mutability::kReadOnly ? "const " : "",
                RenderTypeAsC(unit, p.pointee));
          },
          [](const mir::RuntimeLibraryType& r) -> std::string {
            if (r.kind == mir::RuntimeLibraryKind::kDpiBitChunk) {
              return "svBitVecVal";
            }
            if (r.kind == mir::RuntimeLibraryKind::kDpiLogicChunk) {
              return "svLogicVecVal";
            }
            // LRM Annex H.8.6: an open array is passed by handle in either
            // direction, and the handle carries a `const` qualifier because the
            // foreign side may not modify it.
            if (r.kind == mir::RuntimeLibraryKind::kDpiOpenArrayHandle) {
              return "const svOpenArrayHandle";
            }
            throw InternalError(
                "RenderTypeAsC: this runtime library type does not cross the "
                "DPI-C boundary");
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsC: this type does not cross the DPI-C boundary");
          }},
      unit.types.Get(id).data);
}

// The full C declarator of one foreign callable, the text a user's compiler
// checks their definition or call against. It is the callable's own signature
// spelled in C, so it cannot drift from the one an emitted artifact publishes.
auto RenderPrototype(
    const mir::CompilationUnit& unit, const mir::CallableDecl& callable)
    -> std::string {
  const mir::CallableCode& code = callable.code;
  std::string params;
  for (std::size_t i = 0; i < code.params.size(); ++i) {
    if (i != 0) params += ", ";
    params += RenderTypeAsC(unit, code.locals.Get(code.params[i]).type);
  }
  if (params.empty()) {
    params = "void";
  }
  return std::format(
      "{} {}({})", RenderTypeAsC(unit, code.result_type), callable.LinkedName(),
      params);
}

struct ForeignEntry {
  std::string name;
  std::string prototype;
};

// The program's foreign surface, split by what the C side does with each name:
// it defines an import and calls an export. That is exactly the header's two
// sections, so the split happens once here and neither section filters.
struct ForeignSurface {
  std::vector<ForeignEntry> imports;
  std::vector<ForeignEntry> exports;
};

// The entry already on the surface under this name, whichever section it sits
// in: a foreign name is program-global (LRM 35.4, 35.7), so one name is one
// symbol and the two sections never both hold it.
auto FindOnSurface(const ForeignSurface& surface, std::string_view name)
    -> const ForeignEntry* {
  for (const ForeignEntry& entry : surface.imports) {
    if (entry.name == name) return &entry;
  }
  for (const ForeignEntry& entry : surface.exports) {
    if (entry.name == name) return &entry;
  }
  return nullptr;
}

// Adds one foreign declaration to the surface. A foreign name may be declared
// in several places and each place lowers its own copy, so a repeat is expected
// and collapses to the one symbol the C side sees. LRM 35.5.4 requires every
// such declaration to agree, and the frontend rejects a design where they do
// not, so a disagreement reaching here means an inconsistent surface got past
// that check.
void RecordCallable(
    const mir::CompilationUnit& unit, const mir::CallableDecl& callable,
    ForeignSurface& surface) {
  if (!callable.foreign.has_value()) {
    return;
  }
  ForeignEntry entry{
      .name = callable.LinkedName(),
      .prototype = RenderPrototype(unit, callable)};
  if (const ForeignEntry* seen = FindOnSurface(surface, entry.name);
      seen != nullptr) {
    if (seen->prototype != entry.prototype) {
      throw InternalError(
          std::format(
              "RenderAbiHeader: DPI-C name '{}' reached the ABI surface with "
              "conflicting prototypes '{}' and '{}'",
              entry.name, seen->prototype, entry.prototype));
    }
    return;
  }
  // A callable this design defines is an entry point the C side calls; one it
  // declares without defining is what the C side must define. The presence of a
  // body is what separates them -- neither side carries a tag.
  (callable.code.body.has_value() ? surface.exports : surface.imports)
      .push_back(std::move(entry));
}

// A DPI-C name is program-global and belongs to no class (LRM 35.4, 35.7), so
// the unit's own callables are the whole of its foreign surface.
void CollectUnit(const mir::CompilationUnit& unit, ForeignSurface& surface) {
  for (const mir::CallableDecl& callable : unit.callables) {
    RecordCallable(unit, callable, surface);
  }
}

auto RenderSection(
    std::string_view heading, std::span<const ForeignEntry> section)
    -> std::string {
  std::string out = std::format("/* {} */\n", heading);
  for (const ForeignEntry& entry : section) {
    out += std::format("{};\n", entry.prototype);
  }
  return out + "\n";
}

}  // namespace

auto RenderAbiHeader(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> std::string {
  ForeignSurface surface;
  for (const mir::CompilationUnit& unit : units) {
    CollectUnit(unit, surface);
  }
  CollectUnit(root, surface);

  std::string out;
  out +=
      "/* The DPI-C application binary interface of this design (LRM 35),\n"
      "   generated by Lyra. Include it from the C sources you link with the\n"
      "   design; it is rewritten every time the design is emitted. */\n";
  out += "#ifndef LYRA_DPI_ABI_H\n";
  out += "#define LYRA_DPI_ABI_H\n\n";
  out += "#include <stdint.h>\n\n";
  out += "#include \"svdpi.h\"\n\n";
  out += "#ifdef __cplusplus\n";
  out += "extern \"C\" {\n";
  out += "#endif\n\n";
  out += RenderSection(
      "Imported by the design; define these in your C sources.",
      surface.imports);
  out += RenderSection(
      "Exported by the design; call these from your C sources.",
      surface.exports);
  out += "#ifdef __cplusplus\n";
  out += "}\n";
  out += "#endif\n\n";
  out += "#endif\n";
  return out;
}

}  // namespace lyra::dpi
