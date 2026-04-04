#include "lyra/lowering/hir_to_mir/dpi_header.hpp"

#include <algorithm>
#include <format>
#include <string>
#include <vector>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/dpi_verify.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ClassifyDpiReturnKind(DpiAbiTypeClass abi_type) -> mir::DpiReturnKind {
  if (abi_type == DpiAbiTypeClass::kVoid) return mir::DpiReturnKind::kVoid;
  if (IsPackedVecDpiType(abi_type)) return mir::DpiReturnKind::kIndirect;
  return mir::DpiReturnKind::kDirectValue;
}

// Map a DpiAbiTypeClass to its canonical C type spelling for headers.
auto DpiAbiTypeToCString(DpiAbiTypeClass t) -> const char* {
  switch (t) {
    case DpiAbiTypeClass::kVoid:
      return "void";
    case DpiAbiTypeClass::kBit:
      return "unsigned char";
    case DpiAbiTypeClass::kByte:
      return "char";
    case DpiAbiTypeClass::kShortInt:
      return "short";
    case DpiAbiTypeClass::kInt:
      return "int";
    case DpiAbiTypeClass::kLongInt:
      return "long long";
    case DpiAbiTypeClass::kReal:
      return "double";
    case DpiAbiTypeClass::kShortReal:
      return "float";
    case DpiAbiTypeClass::kString:
      return "const char*";
    case DpiAbiTypeClass::kChandle:
      return "void*";
    case DpiAbiTypeClass::kLogicScalar:
      return "svLogic";
    case DpiAbiTypeClass::kLogicVecNarrow:
    case DpiAbiTypeClass::kLogicVecWide:
      return "svLogicVecVal*";
    case DpiAbiTypeClass::kBitVecWide:
      return "svBitVecVal*";
    case DpiAbiTypeClass::kInvalid:
      throw common::InternalError(
          "DpiAbiTypeToCString", "invalid DPI ABI type class");
  }
  throw common::InternalError(
      "DpiAbiTypeToCString", "unknown DPI ABI type class");
}

// Render one parameter for a C prototype.
auto RenderParam(const mir::DpiParamDesc& p) -> std::string {
  if (p.passing == mir::DpiPassingMode::kByPointer) {
    // Output/inout scalars: pointer to type.
    // Packed vectors: already pointer-typed in the C spelling.
    if (IsPackedVecDpiType(p.abi_type)) {
      if (p.direction == ParameterDirection::kInput) {
        // Input packed vector: const pointer.
        std::string base = DpiAbiTypeToCString(p.abi_type);
        return std::format("const {}", base);
      }
      return DpiAbiTypeToCString(p.abi_type);
    }
    // Scalar output/inout: pointer to scalar type.
    return std::format("{}*", DpiAbiTypeToCString(p.abi_type));
  }
  return DpiAbiTypeToCString(p.abi_type);
}

// Render a full C function prototype line.
// Hidden args are rendered in canonical order: scope (if context), then
// result pointer (if indirect return), then user params.
auto RenderPrototype(
    const std::string& c_name, const mir::DpiSignature& sig, bool is_context)
    -> std::string {
  bool indirect_ret = sig.result.kind == mir::DpiReturnKind::kIndirect;
  std::string ret_ty =
      indirect_ret ? "void" : DpiAbiTypeToCString(sig.result.abi_type);

  std::vector<std::string> params;

  if (is_context) {
    params.emplace_back("svScope scope");
  }
  if (indirect_ret) {
    params.push_back(
        std::format("{} result", DpiAbiTypeToCString(sig.result.abi_type)));
  }
  for (const auto& p : sig.params) {
    params.push_back(RenderParam(p));
  }

  if (params.empty()) {
    return std::format("{} {}(void);", ret_ty, c_name);
  }
  std::string joined;
  for (size_t i = 0; i < params.size(); ++i) {
    if (i > 0) joined += ", ";
    joined += params[i];
  }
  return std::format("{} {}({});", ret_ty, c_name, joined);
}

// Sortable entry for deterministic header output.
struct HeaderEntry {
  std::string c_name;
  mir::DpiSignature signature;
  bool is_export = false;
  bool is_context = false;
};

}  // namespace

auto RenderDpiHeader(
    const DesignDpiExports& exports, const DesignDpiImports& imports)
    -> std::string {
  // Collect all entries.
  std::vector<HeaderEntry> entries;
  for (const auto& [_, info] : exports.Entries()) {
    entries.push_back({
        .c_name = info.c_name,
        .signature = info.signature,
        .is_export = true,
    });
  }
  for (const auto& [_, info] : imports.Entries()) {
    // Build DpiSignature from import info.
    mir::DpiSignature sig;
    sig.result = {
        .sv_type = info.return_type_id,
        .abi_type = info.return_abi_type,
        .kind = ClassifyDpiReturnKind(info.return_abi_type),
    };
    for (const auto& p : info.params) {
      sig.params.push_back({
          .sv_type = p.type_id,
          .abi_type = p.abi_type,
          .direction = p.direction,
          .passing = mir::GetDpiPassingMode(p.direction, p.abi_type),
      });
    }
    entries.push_back({
        .c_name = info.c_name,
        .signature = std::move(sig),
        .is_export = false,
        .is_context = info.is_context,
    });
  }

  // Sort by c_name for deterministic output.
  std::ranges::sort(entries, [](const auto& a, const auto& b) {
    return a.c_name < b.c_name;
  });

  // Render header.
  std::string out;
  out += "#ifndef __LYRA_DPI_H\n";
  out += "#define __LYRA_DPI_H\n\n";
  out += "#include <svdpi.h>\n\n";
  out += "#ifdef __cplusplus\n";
  out += "extern \"C\" {\n";
  out += "#endif\n\n";

  bool has_exports = false;
  bool has_imports = false;
  for (const auto& e : entries) {
    if (e.is_export)
      has_exports = true;
    else
      has_imports = true;
  }

  if (has_exports) {
    out += "/* Exported functions (callable from C) */\n";
    for (const auto& e : entries) {
      if (!e.is_export) continue;
      out += RenderPrototype(e.c_name, e.signature, e.is_context);
      out += "\n";
    }
    out += "\n";
  }

  if (has_imports) {
    out += "/* Imported functions (implemented by C) */\n";
    for (const auto& e : entries) {
      if (e.is_export) continue;
      out += RenderPrototype(e.c_name, e.signature, e.is_context);
      out += "\n";
    }
    out += "\n";
  }

  out += "#ifdef __cplusplus\n";
  out += "}\n";
  out += "#endif\n\n";
  out += "#endif /* __LYRA_DPI_H */\n";

  return out;
}

}  // namespace lyra::lowering::hir_to_mir
