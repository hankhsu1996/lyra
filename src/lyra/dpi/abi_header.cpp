#include "lyra/dpi/abi_header.hpp"

#include <algorithm>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::dpi {

namespace {

// The C spelling of a type that crosses the DPI-C boundary (LRM 35.5.6, Annex
// H) -- the C target's type mapping, the peer of the emitted backend's. The
// set is closed: a foreign signature names only machine scalars, a borrowed
// pointer to one, a canonical vector chunk, or an open-array handle, so
// anything else reaching here is a boundary the lowering should have rejected.
auto RenderTypeAsC(const mir::TypePool& types, mir::TypeId id) -> std::string {
  const auto crosses_no_boundary = []() -> std::string {
    throw InternalError(
        "RenderTypeAsC: this type does not cross the DPI-C boundary -- please "
        "report this as a bug");
  };
  return types.Get(id).Visit(
      Overloaded{
          [](const mir::VoidType&) -> std::string { return "void"; },
          [](const mir::MachineIntType& m) -> std::string {
            const bool is_signed = m.signedness == mir::Signedness::kSigned;
            switch (m.width) {
              case mir::MachineIntWidth::k8:
                return is_signed ? "int8_t" : "uint8_t";
              case mir::MachineIntWidth::k16:
                return is_signed ? "int16_t" : "uint16_t";
              case mir::MachineIntWidth::k32:
                return is_signed ? "int32_t" : "uint32_t";
              case mir::MachineIntWidth::k64:
                return is_signed ? "int64_t" : "uint64_t";
            }
            throw InternalError("RenderTypeAsC: unknown MachineIntWidth");
          },
          [](const mir::MachineFloatType& f) -> std::string {
            switch (f.width) {
              case mir::MachineFloatWidth::k32:
                return "float";
              case mir::MachineFloatWidth::k64:
                return "double";
            }
            throw InternalError("RenderTypeAsC: unknown MachineFloatWidth");
          },
          [](const mir::MachineCStringType&) -> std::string {
            return "const char*";
          },
          [&](const mir::PointerType& p) -> std::string {
            return std::format(
                "{}{}*",
                p.mutability == mir::Mutability::kReadOnly ? "const " : "",
                RenderTypeAsC(types, p.pointee));
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
          // Every other type is one no foreign signature names, so the
          // boundary lowering should have refused the declaration before a
          // header was asked to spell it.
          [&](const mir::PackedArrayType&) { return crosses_no_boundary(); },
          [&](const mir::EnumType&) { return crosses_no_boundary(); },
          [&](const mir::PackedStructType&) { return crosses_no_boundary(); },
          [&](const mir::PackedUnionType&) { return crosses_no_boundary(); },
          [&](const mir::UnpackedArrayType&) { return crosses_no_boundary(); },
          [&](const mir::DynamicArrayType&) { return crosses_no_boundary(); },
          [&](const mir::QueueType&) { return crosses_no_boundary(); },
          [&](const mir::AssociativeArrayType&) {
            return crosses_no_boundary();
          },
          [&](const mir::WildcardIndexType&) { return crosses_no_boundary(); },
          [&](const mir::StringType&) { return crosses_no_boundary(); },
          [&](const mir::MachineBoolType&) { return crosses_no_boundary(); },
          [&](const mir::MachineArrayType&) { return crosses_no_boundary(); },
          [&](const mir::MachineFunctionType&) {
            return crosses_no_boundary();
          },
          [&](const mir::EventType&) { return crosses_no_boundary(); },
          [&](const mir::RealType&) { return crosses_no_boundary(); },
          [&](const mir::ShortRealType&) { return crosses_no_boundary(); },
          [&](const mir::RealTimeType&) { return crosses_no_boundary(); },
          [&](const mir::ChandleType&) { return crosses_no_boundary(); },
          [&](const mir::EmptyType&) { return crosses_no_boundary(); },
          [&](const mir::ObjectType&) { return crosses_no_boundary(); },
          [&](const mir::ExternalUnitObjectType&) {
            return crosses_no_boundary();
          },
          [&](const mir::CrossUnitClassType&) { return crosses_no_boundary(); },
          [&](const mir::OpaqueObjectType&) { return crosses_no_boundary(); },
          [&](const mir::RuntimeClassType&) { return crosses_no_boundary(); },
          [&](const mir::RuntimeEffectsType&) { return crosses_no_boundary(); },
          [&](const mir::FilesType&) { return crosses_no_boundary(); },
          [&](const mir::DiagnosticType&) { return crosses_no_boundary(); },
          [&](const mir::CoroutineType&) { return crosses_no_boundary(); },
          [&](const mir::RefType&) { return crosses_no_boundary(); },
          [&](const mir::ManagedRefType&) { return crosses_no_boundary(); },
          [&](const mir::VectorType&) { return crosses_no_boundary(); },
          [&](const mir::TupleType&) { return crosses_no_boundary(); },
          [&](const mir::UnpackedStructType&) { return crosses_no_boundary(); },
          [&](const mir::UnionType&) { return crosses_no_boundary(); },
          [&](const mir::TaggedUnionType&) { return crosses_no_boundary(); },
          [&](const mir::ObservableType&) { return crosses_no_boundary(); },
          [&](const mir::ResolvedType&) { return crosses_no_boundary(); },
          [&](const mir::DriverType&) { return crosses_no_boundary(); },
          [&](const mir::OpenWriteType&) { return crosses_no_boundary(); },
          [&](const mir::SampledHistoryType&) { return crosses_no_boundary(); },
          [&](const mir::EvaluationAttemptsType&) {
            return crosses_no_boundary();
          },
          [&](const mir::StructType&) { return crosses_no_boundary(); },
          [&](const mir::ClosureType&) { return crosses_no_boundary(); }});
}

// The full C declarator of one foreign name, the text a user's compiler checks
// their definition or call against. The name is the caller's to supply: it is
// the linkage name, which lives in the DPI-C name space (LRM 35.4) rather than
// among the names a unit answers.
//
// The types are the caller's too, because a prototype reaches this from two
// spellings -- an interned function type for an entry that sits on a scope, the
// callable's own signature for a name a unit's namespace owns -- and both are
// the same list. Rendering them in one place is what keeps the two spellings
// from publishing prototypes that differ.
auto RenderPrototype(
    const mir::TypePool& types, mir::TypeId result,
    std::span<const mir::TypeId> params, std::string_view linkage_name)
    -> std::string {
  std::string rendered;
  for (const mir::TypeId param : params) {
    if (!rendered.empty()) rendered += ", ";
    rendered += RenderTypeAsC(types, param);
  }
  if (rendered.empty()) {
    rendered = "void";
  }
  return std::format(
      "{} {}({})", RenderTypeAsC(types, result), linkage_name, rendered);
}

// The declarator of a foreign name whose prototype is an interned function type
// (LRM 35.5.6).
auto RenderPrototypeOfType(
    const mir::TypePool& types, mir::TypeId prototype,
    std::string_view linkage_name) -> std::string {
  const auto& signature = types.Get(prototype).Get<mir::MachineFunctionType>();
  return RenderPrototype(
      types, signature.result, signature.params, linkage_name);
}

// The declarator of a foreign name a callable carries the prototype of, as the
// bindings a body can name rather than as a type.
auto RenderPrototypeOfCallable(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    std::string_view linkage_name) -> std::string {
  std::vector<mir::TypeId> params;
  params.reserve(code.params.size());
  for (const mir::LocalId param : code.params) {
    params.push_back(code.locals.Get(param).type);
  }
  return RenderPrototype(unit.types, code.result_type, params, linkage_name);
}

struct ForeignEntry {
  std::string name;
  std::string prototype;
};

// The unit's foreign surface, split by what the C side does with each name: it
// defines an import and calls an export. That is exactly the fragment's two
// sections, so the split happens once here and neither section filters.
struct ForeignSurface {
  std::vector<ForeignEntry> imports;
  std::vector<ForeignEntry> exports;
};

// Whether this unit already states the name. One declaration is enough for the
// unit, and the front end has already rejected a unit whose declarations of one
// name disagree, so the repeat is nothing to report.
auto AlreadyStated(const ForeignSurface& surface, std::string_view name)
    -> bool {
  const auto named = [&](const ForeignEntry& entry) {
    return entry.name == name;
  };
  return std::ranges::any_of(surface.imports, named) ||
         std::ranges::any_of(surface.exports, named);
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

// Everything the unit states about the foreign name space. A name the unit
// supplies a body for is an entry point the C side calls; one the foreign
// program defines is what the C side must define. An
// entry that sits on a scope is compiled once per specialization of that scope,
// so it is not among the unit's callables and is walked separately.
auto SurfaceOf(const mir::CompilationUnit& unit) -> ForeignSurface {
  ForeignSurface surface;
  for (const mir::CallableDecl& callable : unit.callables) {
    if (!callable.foreign.has_value() ||
        AlreadyStated(surface, callable.foreign->foreign_name)) {
      continue;
    }
    ForeignEntry entry{
        .name = callable.foreign->foreign_name,
        .prototype = RenderPrototypeOfCallable(
            unit, callable.code, callable.foreign->foreign_name)};
    std::vector<ForeignEntry>& side = std::visit(
        Overloaded{
            [&](mir::DefinedHere) -> std::vector<ForeignEntry>& {
              return surface.exports;
            },
            [&](mir::DefinedByForeignCode) -> std::vector<ForeignEntry>& {
              return surface.imports;
            },
            [](mir::LeftAbstract) -> std::vector<ForeignEntry>& {
              throw InternalError(
                  "dpi: a callable of a unit's namespace is left abstract, "
                  "which only a class's can be -- please report this as a bug");
            }},
        mir::FormOf(callable));
    side.push_back(std::move(entry));
  }
  for (const mir::ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    if (AlreadyStated(surface, entry.linkage.foreign_name)) {
      continue;
    }
    surface.exports.push_back(
        ForeignEntry{
            .name = entry.linkage.foreign_name,
            .prototype = RenderPrototypeOfType(
                unit.types, entry.signature, entry.linkage.foreign_name)});
  }
  return surface;
}

}  // namespace

auto AbiFragmentOf(const mir::CompilationUnit& unit)
    -> std::optional<std::string> {
  const ForeignSurface surface = SurfaceOf(unit);
  if (surface.imports.empty() && surface.exports.empty()) {
    return std::nullopt;
  }
  std::string text;
  text += std::format(
      "/* What '{}' states of this design's DPI-C boundary (LRM 35),\n"
      "   generated by Lyra. Reached through the design's own DPI header. */\n",
      unit.name);
  text += RenderSection(
      "Imported by the design; define these in your C sources.",
      surface.imports);
  text += RenderSection(
      "Exported by the design; call these from your C sources.",
      surface.exports);
  return text;
}

void AddAbiFragment(std::vector<AbiFragment>& fragments, std::string text) {
  fragments.push_back(
      AbiFragment{
          .relpath = std::format("dpi_{}.h", fragments.size()),
          .text = std::move(text)});
}

auto RenderAbiHeader(std::span<const AbiFragment> fragments) -> std::string {
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
  for (const AbiFragment& fragment : fragments) {
    out += std::format("#include \"{}\"\n", fragment.relpath);
  }
  out += "\n#ifdef __cplusplus\n";
  out += "}\n";
  out += "#endif\n\n";
  out += "#endif\n";
  return out;
}

}  // namespace lyra::dpi
