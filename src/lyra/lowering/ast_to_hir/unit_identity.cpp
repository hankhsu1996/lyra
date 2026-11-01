#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <limits>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/parsing/TokenKind.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/frontend/library_texts.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/lowering/ast_to_hir/outside_reach.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

auto NameDigest(std::string_view bytes) -> std::uint64_t {
  const stable_hash::Fingerprint fingerprint =
      stable_hash::FingerprintOfText(bytes);
  std::uint64_t digest = 0;
  for (std::size_t i = 0; i < sizeof(digest); ++i) {
    digest = (digest << 8U) | fingerprint.bytes.at(i);
  }
  return digest;
}

namespace {

// A name and the digest of what was fixed under it, as one bounded identifier:
// the name alone where nothing was fixed.
auto WithDigest(std::string name, const std::optional<std::uint64_t>& digest)
    -> std::string {
  if (digest.has_value()) name += std::format("__{:016x}", *digest);
  return name;
}

}  // namespace

namespace {

// Whether `type` is one the source declares -- a class, a covergroup, an
// enumeration, a structure or a union, packed or not, named or written in
// place -- which is one type per declaration however alike two are written
// (LRM 6.22.1 c, d, h; 8.3).
auto IsIdentifiedByDeclaration(const slang::ast::Type& type) -> bool {
  using slang::ast::SymbolKind;
  switch (type.kind) {
    case SymbolKind::ClassType:
    case SymbolKind::CovergroupType:
    case SymbolKind::EnumType:
    case SymbolKind::PackedStructType:
    case SymbolKind::UnpackedStructType:
    case SymbolKind::PackedUnionType:
    case SymbolKind::UnpackedUnionType:
      return true;
    default:
      return false;
  }
}

// One type's identity. A declared type answers with the unit declaring it and
// which declaration of that unit it is. A type built out of others answers with
// its own form over the identities of what it holds (LRM 6.22.1 f), and a
// built-in type with its keyword (LRM 6.22.1 a).
//
// Every arm answers with an identity rather than adding to one under
// construction, so a form this does not spell cannot contribute nothing and
// leave two types looking alike.
auto TypeIdentity(const slang::ast::Type& type, const InstanceNames& names)
    -> std::string {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (IsIdentifiedByDeclaration(canonical)) {
    const std::string unit = CompilationUnitName(UnitHomeOf(canonical), names);
    return std::format(
        "declared {}:{} {}", unit.size(), unit,
        support::DefPathIdentity(DefPathOf(canonical, names)));
  }

  switch (canonical.kind) {
    case SymbolKind::ScalarType:
    case SymbolKind::PredefinedIntegerType:
    case SymbolKind::FloatingType:
    case SymbolKind::StringType:
    case SymbolKind::CHandleType:
    case SymbolKind::EventType:
    case SymbolKind::VoidType:
    // What the front end gives a mailbox's message type where none was stated
    // (LRM 15.4.9 `dynamic_type`).
    case SymbolKind::UntypedType:
      return canonical.toString();
    case SymbolKind::PackedArrayType: {
      const auto& array = canonical.as<slang::ast::PackedArrayType>();
      return std::format(
          "packed[{}:{}]{}", array.range.left, array.range.right,
          TypeIdentity(array.elementType, names));
    }
    case SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& array =
          canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      return std::format(
          "[{}:{}]{}", array.range.left, array.range.right,
          TypeIdentity(array.elementType, names));
    }
    case SymbolKind::DynamicArrayType:
      return "[]" +
             TypeIdentity(
                 canonical.as<slang::ast::DynamicArrayType>().elementType,
                 names);
    case SymbolKind::QueueType: {
      const auto& queue = canonical.as<slang::ast::QueueType>();
      return std::format(
          "[${}]{}", queue.maxBound, TypeIdentity(queue.elementType, names));
    }
    case SymbolKind::AssociativeArrayType: {
      const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
      const std::string index = assoc.indexType == nullptr
                                    ? std::string{"*"}
                                    : TypeIdentity(*assoc.indexType, names);
      return std::format(
          "[{}]{}", index, TypeIdentity(assoc.elementType, names));
    }
    // LRM 25.9: the type names the interface together with the parameters it
    // was given, and the view it is taken through.
    case SymbolKind::VirtualInterfaceType: {
      const auto& handle = canonical.as<slang::ast::VirtualInterfaceType>();
      const std::string unit = names.InterfaceNameOf(handle.iface);
      return std::format(
          "virtual {}:{} {}", unit.size(), unit,
          handle.modport == nullptr ? std::string_view{}
                                    : handle.modport->name);
    }
    default:
      throw InternalError(
          std::format(
              "TypeIdentity: a {} is no type a parameter is fixed to",
              slang::ast::toString(canonical.kind)));
  }
}

}  // namespace

// Every spelling ends where it can be seen to end, so one joined into an
// aggregate cannot run into its neighbour. A real is spelled by its bits,
// because a decimal rendering spells every NaN alike; a string by its length
// and its text, because a quote and a comma are ordinary characters in one; an
// aggregate through its elements, so each of those holds inside it too.
auto ValueIdentity(const slang::ConstantValue& value) -> std::string {
  if (value.isString()) {
    return std::format("t{}:{}", value.str().size(), value.str());
  }
  if (value.isReal()) {
    return std::format(
        "r{:016x}", std::bit_cast<std::uint64_t>(double{value.real()}));
  }
  if (value.isShortReal()) {
    return std::format(
        "s{:08x}", std::bit_cast<std::uint32_t>(float{value.shortReal()}));
  }
  const auto elements = [](const auto& values) {
    std::string out{'['};
    for (const slang::ConstantValue& element : values) {
      out += ValueIdentity(element);
      out += ',';
    }
    return out + ']';
  };
  if (value.isUnpacked()) {
    return elements(value.elements());
  }
  if (value.isQueue()) {
    return "q" + elements(*value.queue());
  }
  if (value.isMap()) {
    std::string out{"m["};
    for (const auto& [key, element] : *value.map()) {
      out += std::format("{}:{},", ValueIdentity(key), ValueIdentity(element));
    }
    if (value.map()->defaultValue) {
      out += "default:" + ValueIdentity(value.map()->defaultValue);
    }
    return out + ']';
  }
  if (value.isUnion()) {
    const auto& u = *value.unionVal();
    return u.activeMember.has_value()
               ? std::format("u{}:{}", *u.activeMember, ValueIdentity(u.value))
               : std::string{"u-"};
  }
  // An integer by its width, how it is read, and its words. Written as digits
  // it would cost the square of its width, which a parameter of a wide type
  // then makes the cost of naming every unit it reaches.
  if (value.isInteger()) {
    const slang::SVInt& integer = value.integer();
    std::string out = std::format(
        "i{}{}{}:", integer.getBitWidth(), integer.isSigned() ? 's' : 'u',
        integer.hasUnknown() ? 'x' : 'b');
    const std::span<const std::uint64_t> words(
        integer.getRawPtr(), integer.getNumWords());
    for (const std::uint64_t word : words) {
      out += std::format("{:016x}", word);
    }
    return out;
  }
  constexpr slang::bitwidth_t kNeverShortened =
      std::numeric_limits<slang::bitwidth_t>::max();
  constexpr bool kExactUnknowns = true;
  return value.toString(kNeverShortened, kExactUnknowns);
}

namespace {

// What one parameter was fixed to. A parameter is fixed to a value or to a type
// (LRM 6.20.2, 6.20.3), and that is the whole of the split. The parameter
// arrives as its concrete Symbol so this serves both spaces the frontend
// exposes bindings through -- a module body's parameter list and a class
// specialization's generic parameters.
auto ParameterInput(
    const slang::ast::Symbol& symbol, const InstanceNames& names)
    -> ClassKeyInput {
  if (symbol.kind == slang::ast::SymbolKind::Parameter) {
    return ClassKeyInput{
        .name = std::string{symbol.name},
        .kind = BoundValue{
            .value = ValueIdentity(
                symbol.as<slang::ast::ParameterSymbol>().getValue())}};
  }
  return ClassKeyInput{
      .name = std::string{symbol.name},
      .kind = BoundType{
          .type = TypeIdentity(
              symbol.as<slang::ast::TypeParameterSymbol>().targetType.getType(),
              names)}};
}

// Whether `symbol` is the scope a compilation unit is: a package (LRM 26), a
// design element's body (LRM 23.2, 25), or the `$unit` file-set scope (LRM
// 3.12.1).
auto IsCompilationUnit(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::Package ||
         symbol.kind == slang::ast::SymbolKind::InstanceBody ||
         symbol.kind == slang::ast::SymbolKind::CompilationUnit;
}

// The bytes what parameters were bound to folds to. Every part is written with
// its own delimiter, so two lists that differ anywhere differ here; nothing
// rests on this being read back, and nothing compares keys through it -- a key
// answers that itself.
auto InputBytes(std::span<const ClassKeyInput> inputs) -> std::string {
  std::string bytes;
  for (const ClassKeyInput& input : inputs) {
    bytes += input.name;
    bytes += '=';
    bytes += std::visit(
        Overloaded{
            [](const BoundValue& v) { return v.value; },
            [](const BoundType& v) { return v.type; }},
        input.kind);
    bytes += ';';
  }
  return bytes;
}

}  // namespace

auto IsInstanceScope(const slang::ast::Scope& scope) -> bool {
  const slang::ast::SymbolKind kind = scope.asSymbol().kind;
  return kind == slang::ast::SymbolKind::InstanceBody ||
         kind == slang::ast::SymbolKind::GenerateBlock;
}

auto InstantiationOf(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol& {
  if (body.parentInstance == nullptr) {
    throw InternalError(
        "InstantiationOf: a body is elaborated for an application of a "
        "definition, so one that belongs to none was never built");
  }
  return *body.parentInstance;
}

namespace {

// The body `scope` is or stands in.
auto InstanceBodyHolding(const slang::ast::Scope& scope)
    -> const slang::ast::InstanceBodySymbol& {
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (const auto* body =
            level->asSymbol().as_if<slang::ast::InstanceBodySymbol>()) {
      return *body;
    }
  }
  throw InternalError(
      "InstanceBodyHolding: the scope stands in the body of no instance");
}

}  // namespace

auto InstanceHolding(const slang::ast::Scope& scope)
    -> const slang::ast::InstanceSymbol& {
  return InstantiationOf(InstanceBodyHolding(scope));
}

auto DefinitionNameOf(const slang::ast::InstanceSymbol& inst) -> std::string {
  const slang::ast::DefinitionSymbol& definition = inst.getDefinition();
  if (IsLibraryCell(definition)) return CellName(definition);

  // A nested declaration is one of the body declaring it, and that body is one
  // the instance stands in, since the name is visible nowhere else.
  return std::format(
      "{}::{}", DefinitionNameOf(InstantiationOf(BodyDeclaring(definition))),
      definition.name);
}

auto PathStepOf(const slang::ast::GenerateBlockSymbol& block)
    -> support::GenerateBlockStep {
  const std::optional<std::uint64_t> index =
      block.getArrayIndex() == nullptr
          ? std::nullopt
          : std::optional{static_cast<std::uint64_t>(LoopIndexOf(block))};
  return support::GenerateBlockStep{
      .label = GenerateBlockLabel(block),
      .disambiguator = LabelDisambiguatorOf(block),
      .distinguisher = index};
}

auto PathNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  return inst.getHierarchicalPath();
}

auto PathNames::InterfaceNameOf(const slang::ast::InstanceSymbol& iface) const
    -> std::string {
  return NameOf(iface);
}

auto PathNames::BlockStepOf(const slang::ast::GenerateBlockSymbol& block) const
    -> support::GenerateBlockStep {
  return PathStepOf(block);
}

auto ArgumentsDigest(const ClassKey& key) -> std::optional<std::uint64_t> {
  if (key.inputs.empty()) return std::nullopt;
  return NameDigest(InputBytes(key.inputs));
}

auto UnitName(const ClassKey& key) -> std::string {
  return WithDigest(key.definition, ArgumentsDigest(key));
}

namespace {

// The key of the class `cls` under `definition`, the name its declaration is
// told from others by wherever the key is used.
auto ClassKeyUnder(
    std::string definition, const slang::ast::ClassType& cls,
    const InstanceNames& names) -> ClassKey {
  ClassKey key{.definition = std::move(definition), .inputs = {}};
  if (cls.genericClass == nullptr) {
    return key;
  }
  for (const auto* sym : cls.genericParameters) {
    key.inputs.push_back(ParameterInput(*sym, names));
  }
  return key;
}

// A class is declared in a design element, a package, a generate block or
// another class (LRM 8.3, A.1.4, A.1.9), so no other scope stands above one.
[[noreturn]] void ThrowClassDeclaredIn(std::string_view what) {
  throw InternalError(
      std::format("ClassKeyOf: a class is declared inside {}", what));
}

}  // namespace

auto ClassKeyOf(const slang::ast::ClassType& cls, const InstanceNames& names)
    -> ClassKey {
  // The key names a class inside its compilation unit by one string, and the
  // source name alone does not: SystemVerilog scopes the declaration, so
  // sibling generate blocks may each declare the same one (LRM 27.3), and so
  // may two classes (LRM 8.23).
  const support::DefPath path = DefPathOf(cls, names);
  std::string definition;
  for (const support::DefPathData& enclosing :
       std::span{path.data}.first(path.data.size() - 1)) {
    definition += std::visit(
        Overloaded{
            [](const support::GenerateBlockStep& block) -> std::string {
              std::string text = block.label;
              if (block.disambiguator != 0) {
                text += std::format("#{}", block.disambiguator);
              }
              return WithDigest(std::move(text), block.distinguisher);
            },
            [](const support::ClassStep& outer) -> std::string {
              return WithDigest(outer.name, outer.arguments);
            },
            [](const support::SubroutineStep&) -> std::string {
              ThrowClassDeclaredIn("a subroutine");
            },
            [](const support::NamedBlockStep&) -> std::string {
              ThrowClassDeclaredIn("a procedural block");
            },
            [](const support::UnnamedBlockStep&) -> std::string {
              ThrowClassDeclaredIn("a procedural block");
            },
            [](const support::TypeStep&) -> std::string {
              ThrowClassDeclaredIn("a structure or a union");
            },
            [](const support::UnnamedTypeStep&) -> std::string {
              ThrowClassDeclaredIn("a structure or a union");
            }},
        enclosing);
    definition += '_';
  }
  definition += cls.name;
  return ClassKeyUnder(std::move(definition), cls, names);
}

auto UnitName(const slang::ast::ClassType& cls, const InstanceNames& names)
    -> std::string {
  return UnitName(ClassKeyOf(cls, names));
}

auto InterfaceIdentity(
    const slang::ast::InstanceSymbol& iface, const InstanceNames& names)
    -> std::string {
  std::string identity = DefinitionNameOf(iface);
  const slang::ast::DefinitionSymbol& definition = iface.getDefinition();
  // A definition declared inside another is one of the body declaring it (LRM
  // 23.4), and two such bodies each declare one.
  if (!IsLibraryCell(definition)) {
    identity += std::format(
        " in {}", names.NameOf(InstantiationOf(BodyDeclaring(definition))));
  }
  std::vector<ClassKeyInput> inputs;
  for (const auto* parameter : iface.body.getParameters()) {
    inputs.push_back(ParameterInput(parameter->symbol, names));
  }
  return std::format("{}({})", identity, InputBytes(inputs));
}

auto DeclaringNamespaceUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol* {
  const slang::ast::Scope* scope = decl.getParentScope();
  while (scope != nullptr &&
         scope->asSymbol().kind == slang::ast::SymbolKind::AnonymousProgram) {
    scope = scope->asSymbol().getParentScope();
  }
  if (scope == nullptr) return nullptr;
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::Package &&
      owner.kind != slang::ast::SymbolKind::CompilationUnit) {
    return nullptr;
  }
  return &owner;
}

auto DeclaringCompilationUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol& {
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& owner = scope->asSymbol();
    if (IsCompilationUnit(owner)) {
      return owner;
    }
  }
  throw InternalError(
      "DeclaringCompilationUnit: every declaration lies in a package, a design "
      "element's body, or the file-set scope");
}

namespace {

auto Encloses(const slang::ast::Scope& outer, const slang::ast::Scope& inner)
    -> bool {
  for (const slang::ast::Scope* level = &inner; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (level == &outer) return true;
  }
  return false;
}

// Of two scopes replicating parts of one type, the one replicating the whole:
// an instance's scope over a namespace's, and the inner of two instances'
// scopes. Two instance scopes neither of which encloses the other cannot both
// reach one type, since a type an instance declares is named only inside it
// (LRM 6.22).
auto Innermost(const slang::ast::Scope* held, const slang::ast::Scope* other)
    -> const slang::ast::Scope* {
  if (other == nullptr || !IsInstanceScope(*other)) return held;
  if (!IsInstanceScope(*held) || Encloses(*held, *other)) return other;
  if (Encloses(*other, *held)) return held;
  throw InternalError(
      "ReplicatingScope: two instance scopes neither enclosing the other "
      "replicate parts of one type");
}

auto ReplicatingScopeOfType(const slang::ast::Type& type)
    -> const slang::ast::Scope* {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (IsIdentifiedByDeclaration(canonical)) return &ReplicatingScope(canonical);
  if (canonical.kind == SymbolKind::AssociativeArrayType) {
    const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
    const slang::ast::Scope* element =
        ReplicatingScopeOfType(assoc.elementType);
    if (assoc.indexType == nullptr) return element;
    const slang::ast::Scope* index = ReplicatingScopeOfType(*assoc.indexType);
    if (element == nullptr) return index;
    return Innermost(element, index);
  }
  if (const slang::ast::Type* element = canonical.getArrayElementType()) {
    return ReplicatingScopeOfType(*element);
  }
  return nullptr;
}

// The innermost specialization of a generic class `decl` is or lies in, short
// of its compilation unit.
auto InnermostSpecialization(const slang::ast::Symbol& decl)
    -> const slang::ast::ClassType* {
  for (const slang::ast::Symbol* level = &decl;
       level != nullptr && !IsCompilationUnit(*level);
       level = level->getParentScope() == nullptr
                   ? nullptr
                   : &level->getParentScope()->asSymbol()) {
    const auto* cls = level->as_if<slang::ast::ClassType>();
    if (cls != nullptr && cls->genericClass != nullptr) return cls;
  }
  return nullptr;
}

}  // namespace

auto ReplicatingScope(const slang::ast::Symbol& decl)
    -> const slang::ast::Scope& {
  const slang::ast::Scope* replicating = nullptr;
  std::vector<const slang::ast::ClassType*> specializations;
  for (const slang::ast::Symbol* level = &decl; replicating == nullptr;) {
    if (const auto* cls = level->as_if<slang::ast::ClassType>();
        cls != nullptr && cls->genericClass != nullptr) {
      specializations.push_back(cls);
    }
    const slang::ast::Scope* parent = level->getParentScope();
    if (parent == nullptr) {
      throw InternalError(
          "ReplicatingScope: every declaration lies inside a structural "
          "scope");
    }
    const slang::ast::SymbolKind kind = parent->asSymbol().kind;
    if (kind == slang::ast::SymbolKind::InstanceBody ||
        kind == slang::ast::SymbolKind::GenerateBlock ||
        kind == slang::ast::SymbolKind::Package ||
        kind == slang::ast::SymbolKind::CompilationUnit) {
      replicating = parent;
    }
    level = &parent->asSymbol();
  }
  const slang::ast::Scope* innermost = replicating;
  for (const slang::ast::ClassType* spec : specializations) {
    for (const auto* param : spec->genericParameters) {
      if (const auto* type_param =
              param->as_if<slang::ast::TypeParameterSymbol>()) {
        innermost = Innermost(
            innermost,
            ReplicatingScopeOfType(type_param->targetType.getType()));
      }
    }
  }
  return *innermost;
}

auto UnitHomeOf(const slang::ast::Symbol& decl) -> const slang::ast::Symbol& {
  const slang::ast::Scope& replicating = ReplicatingScope(decl);
  if (IsInstanceScope(replicating)) return InstanceBodyHolding(replicating);
  if (const slang::ast::ClassType* spec = InnermostSpecialization(decl)) {
    return *spec;
  }
  return DeclaringCompilationUnit(decl);
}

auto IsDesignElement(const slang::ast::Symbol& unit) -> bool {
  return unit.kind == slang::ast::SymbolKind::InstanceBody;
}

auto BelongsToAnInstance(const slang::ast::Symbol& type) -> bool {
  return IsInstanceScope(ReplicatingScope(type));
}

auto CompilationUnitName(
    const slang::ast::Symbol& unit, const InstanceNames& names) -> std::string {
  using slang::ast::SymbolKind;
  if (unit.kind == SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == SymbolKind::InstanceBody) {
    return names.NameOf(
        InstantiationOf(unit.as<slang::ast::InstanceBodySymbol>()));
  }
  if (const auto* spec = unit.as_if<slang::ast::ClassType>()) {
    return std::format(
        "{}__{}", CompilationUnitName(DeclaringCompilationUnit(*spec), names),
        UnitName(*spec, names));
  }
  if (unit.kind == SymbolKind::CompilationUnit) {
    // The anonymous $unit scope has no source name; its distinguishing identity
    // is the compilation-unit input it belongs to, named by the source buffer
    // its declarations live in. Folding the resolved path yields a name stable
    // across edits to the scope's body (the property a module's specialization
    // name has) and distinct per input, with no shared table. A text of Lyra's
    // own is an input that names itself.
    const auto& cu = unit.as<slang::ast::CompilationUnitSymbol>();
    if (const frontend::LibraryText* text = frontend::LibraryTextOf(cu)) {
      return std::string(text->unit);
    }
    const slang::SourceManager* sources =
        cu.getCompilation().getSourceManager();
    if (sources == nullptr) {
      throw InternalError(
          "CompilationUnitName: compilation has no source manager");
    }
    const std::optional<slang::BufferID> buffer = frontend::BufferOf(cu);
    if (!buffer.has_value()) {
      throw InternalError(
          "CompilationUnitName: compilation unit has no located member");
    }
    return std::format(
        "$unit__{:016x}", NameDigest(sources->getFullPath(*buffer).string()));
  }
  throw InternalError(
      "CompilationUnitName: symbol is not a package, module body, compilation "
      "unit, or specialization");
}

namespace {

// Whether `candidate` is the type the declaration `type` came from declares. A
// type written in place of a name is one type for every data object its
// declaration statement declares (LRM 6.22.1 c), while the front end elaborates
// it once per object; what the objects share is the text that wrote it.
auto DeclaredBySameText(
    const slang::ast::Type& candidate, const slang::ast::Type& type) -> bool {
  const slang::ast::Type& canonical = candidate.getCanonicalType();
  return &canonical == &type || (type.getSyntax() != nullptr &&
                                 canonical.getSyntax() == type.getSyntax());
}

// The step a type is in the scope that declares it: under the name it answers
// to there -- the typedef declaring it, or else the first data object its
// declaration statement declares (LRM 6.22.1 c) -- and where nothing declares
// it by name, by where it sits.
auto TypeStepOf(const slang::ast::Type& type) -> support::DefPathData {
  for (const auto& member : type.getParentScope()->members()) {
    if (const auto* alias = member.as_if<slang::ast::TypeAliasType>()) {
      if (DeclaredBySameText(alias->targetType.getType(), type)) {
        return support::TypeStep{.name = std::string{alias->name}};
      }
      continue;
    }
    if (const auto* value = member.as_if<slang::ast::ValueSymbol>()) {
      if (DeclaredBySameText(value->getType(), type)) {
        return support::TypeStep{.name = std::string{value->name}};
      }
    }
  }
  return support::UnnamedTypeStep{
      .position = static_cast<std::uint32_t>(type.getIndex())};
}

// The step `scope` is on the path to whatever it holds, or nothing for a scope
// no declaration stands in: a loop generate, whose blocks each already answer
// to the loop's label (LRM 27.4), and an anonymous program, whose items share
// the name space of the scope around it (LRM 24.6).
auto StepOf(const slang::ast::Symbol& scope, const InstanceNames& names)
    -> std::optional<support::DefPathData> {
  using slang::ast::SymbolKind;
  if (scope.kind == SymbolKind::GenerateBlockArray ||
      scope.kind == SymbolKind::AnonymousProgram) {
    return std::nullopt;
  }
  if (const auto* block = scope.as_if<slang::ast::GenerateBlockSymbol>()) {
    return names.BlockStepOf(*block);
  }
  // A class is told from the others of its scope by its name and, for a
  // specialization of a generic class, what its parameters were bound to
  // (LRM 8.25).
  if (const auto* cls = scope.as_if<slang::ast::ClassType>()) {
    return support::ClassStep{
        .name = std::string{cls->name},
        .arguments = ArgumentsDigest(
            ClassKeyUnder(std::string{cls->name}, *cls, names))};
  }
  // A covergroup is a type of its scope as a class is (LRM 19.3), in the same
  // name space, so it is told from the others by its name the same way.
  if (const auto* covergroup = scope.as_if<slang::ast::CovergroupType>()) {
    return support::ClassStep{
        .name = std::string{CovergroupNameOf(*covergroup)},
        .arguments = std::nullopt};
  }
  if (scope.kind == SymbolKind::Subroutine ||
      scope.kind == SymbolKind::StatementBlock) {
    return ProceduralStepOf(scope);
  }
  if (scope.kind == SymbolKind::EnumType ||
      scope.kind == SymbolKind::PackedStructType ||
      scope.kind == SymbolKind::PackedUnionType ||
      scope.kind == SymbolKind::UnpackedStructType ||
      scope.kind == SymbolKind::UnpackedUnionType) {
    return TypeStepOf(scope.as<slang::ast::Type>());
  }
  throw InternalError(
      std::format(
          "DefPathOf: a {} is no scope a declaration of a unit is identified "
          "through",
          slang::ast::toString(scope.kind)));
}

}  // namespace

auto ExternDefinitionPrefix(const slang::ast::SubroutineSymbol& subroutine)
    -> std::optional<std::string_view> {
  const auto* syntax =
      subroutine.getSyntax() == nullptr
          ? nullptr
          : subroutine.getSyntax()
                ->as_if<slang::syntax::FunctionDeclarationSyntax>();
  const auto* scoped =
      syntax == nullptr
          ? nullptr
          : syntax->prototype->name->as_if<slang::syntax::ScopedNameSyntax>();
  if (scoped == nullptr ||
      scoped->separator.kind != slang::parsing::TokenKind::Dot) {
    return std::nullopt;
  }
  return scoped->left->getFirstToken().valueText();
}

auto SubroutineNameOf(const slang::ast::SubroutineSymbol& subroutine)
    -> std::string {
  const std::optional<std::string_view> prefix =
      ExternDefinitionPrefix(subroutine);
  return prefix.has_value() ? std::format("{} {}", *prefix, subroutine.name)
                            : std::string{subroutine.name};
}

auto ProceduralStepOf(const slang::ast::Symbol& scope) -> support::DefPathData {
  if (scope.kind == slang::ast::SymbolKind::Subroutine) {
    return support::SubroutineStep{
        .name = SubroutineNameOf(scope.as<slang::ast::SubroutineSymbol>())};
  }
  if (scope.kind != slang::ast::SymbolKind::StatementBlock) {
    throw InternalError(
        std::format(
            "ProceduralStepOf: a {} is neither a subroutine nor a block of "
            "statements",
            slang::ast::toString(scope.kind)));
  }
  if (scope.name.empty()) {
    return support::UnnamedBlockStep{
        .position = static_cast<std::uint32_t>(scope.getIndex())};
  }
  return support::NamedBlockStep{.name = std::string{scope.name}};
}

auto CovergroupNameOf(const slang::ast::CovergroupType& covergroup)
    -> std::string_view {
  if (!covergroup.name.empty()) return covergroup.name;
  const auto& embedding =
      covergroup.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  for (const auto& property :
       embedding.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (&property.getType().getCanonicalType() == &covergroup) {
      return property.name;
    }
  }
  throw InternalError(
      "CovergroupNameOf: a covergroup a class embeds is the type of one of its "
      "properties");
}

auto DefPathOf(
    const slang::ast::Symbol& declaration, const InstanceNames& names)
    -> support::DefPath {
  support::DefPath path;
  for (const slang::ast::Symbol* at = &declaration; !IsCompilationUnit(*at);) {
    if (auto step = StepOf(*at, names)) {
      path.data.push_back(*std::move(step));
    }
    const slang::ast::Scope* holder = at->getParentScope();
    if (holder == nullptr) {
      throw InternalError(
          "DefPathOf: every declaration lies in a package, a design element's "
          "body, or the file-set scope");
    }
    at = &holder->asSymbol();
  }
  std::ranges::reverse(path.data);
  return path;
}

}  // namespace lyra::lowering::ast_to_hir
