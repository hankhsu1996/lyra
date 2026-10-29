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
#include <unordered_map>
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
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/frontend/library_texts.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/instance_context.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
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

auto DefPathIdentity(const support::DefPath& path) -> std::string {
  const auto named = [](char kind, std::string_view name) {
    return std::format("{}{}:{}", kind, name.size(), name);
  };
  const auto placed = [](char kind, std::uint32_t position) {
    return std::format("{}{};", kind, position);
  };
  const auto bound = [](const std::optional<std::uint64_t>& arguments) {
    return arguments.has_value() ? std::format("#{:016x};", *arguments)
                                 : std::string{};
  };
  std::string out;
  for (const support::DefPathData& step : path.data) {
    out += std::visit(
        Overloaded{
            [&](const support::GenerateBlockStep& block) {
              std::string step = named('b', block.label);
              if (block.disambiguator != 0) {
                step += std::format("~{};", block.disambiguator);
              }
              return step + bound(block.arguments);
            },
            [&](const support::ClassStep& cls) {
              return named('c', cls.name) + bound(cls.arguments);
            },
            [&](const support::SubroutineStep& subroutine) {
              return named('s', subroutine.name);
            },
            [&](const support::NamedBlockStep& block) {
              return named('n', block.name);
            },
            [&](const support::UnnamedBlockStep& block) {
              return placed('u', block.position);
            },
            [&](const support::TypeStep& type) {
              return named('t', type.name);
            },
            [&](const support::UnnamedTypeStep& type) {
              return placed('a', type.position);
            }},
        step);
  }
  return out;
}

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

// The compilation unit `unit`, as part of a key being worked out. An instance
// whose own key is in progress has no name to give, and what is being worked
// out may be asked about something it holds itself: a type it declares and
// fixes a local parameter to (LRM 6.20.4), or a design element nested in it
// (LRM 23.4). Such an instance is told by how far out on the chain of keys in
// progress it stands, which is all two instances could differ in there.
auto UnitWhileNaming(
    const slang::ast::Symbol& unit, const UnitNamingPolicy& policy)
    -> std::string {
  if (const auto* body = unit.as_if<slang::ast::InstanceBodySymbol>()) {
    if (const auto levels = policy.Keys().LevelsOutTo(*body)) {
      return std::format("^{}", *levels);
    }
  }
  return CompilationUnitName(unit, policy);
}

// One type's identity. A declared type answers with the unit declaring it and
// which declaration of that unit it is. A type built out of others answers with
// its own form over the identities of what it holds (LRM 6.22.1 f), and a
// built-in type with its keyword (LRM 6.22.1 a).
//
// Every arm answers with an identity rather than adding to one under
// construction, so a form this does not spell cannot contribute nothing and
// leave two types looking alike.
auto TypeIdentity(const slang::ast::Type& type, const UnitNamingPolicy& policy)
    -> std::string {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (IsIdentifiedByDeclaration(canonical)) {
    const std::string unit = UnitWhileNaming(UnitHomeOf(canonical), policy);
    return std::format(
        "declared {}:{} {}", unit.size(), unit,
        DefPathIdentity(DefPathOf(canonical, policy)));
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
          TypeIdentity(array.elementType, policy));
    }
    case SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& array =
          canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      return std::format(
          "[{}:{}]{}", array.range.left, array.range.right,
          TypeIdentity(array.elementType, policy));
    }
    case SymbolKind::DynamicArrayType:
      return "[]" +
             TypeIdentity(
                 canonical.as<slang::ast::DynamicArrayType>().elementType,
                 policy);
    case SymbolKind::QueueType: {
      const auto& queue = canonical.as<slang::ast::QueueType>();
      return std::format(
          "[${}]{}", queue.maxBound, TypeIdentity(queue.elementType, policy));
    }
    case SymbolKind::AssociativeArrayType: {
      const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
      const std::string index = assoc.indexType == nullptr
                                    ? std::string{"*"}
                                    : TypeIdentity(*assoc.indexType, policy);
      return std::format(
          "[{}]{}", index, TypeIdentity(assoc.elementType, policy));
    }
    // LRM 25.9: the type names the interface together with the parameters it
    // was given, and the view it is taken through.
    case SymbolKind::VirtualInterfaceType: {
      const auto& handle = canonical.as<slang::ast::VirtualInterfaceType>();
      const std::string unit = policy.NameOf(handle.iface);
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
//
// Reading the settled value is not a choice about where it enters the artifact
// here: what the value varies with is the specialization, and the
// specialization is what this computes.
auto ParameterInput(
    const slang::ast::Symbol& symbol, const UnitNamingPolicy& policy)
    -> UnitKeyInput {
  if (symbol.kind == slang::ast::SymbolKind::Parameter) {
    return UnitKeyInput{
        .name = std::string{symbol.name},
        .kind = FixedValue{
            .value = ValueIdentity(
                symbol.as<slang::ast::ParameterSymbol>().getValue())}};
  }
  return UnitKeyInput{
      .name = std::string{symbol.name},
      .kind = FixedType{
          .type = TypeIdentity(
              symbol.as<slang::ast::TypeParameterSymbol>().targetType.getType(),
              policy)}};
}

// Which interface an interface port carries (LRM 25.3), named by the
// specialization that interface instance is. Everything reached through the
// port takes its types and positions from there, so two instantiations bound to
// different interfaces build different objects, exactly as two parameter
// bindings do. A port carrying a range carries one interface per
// element, each its own, so the answer is which unit stands at each position.
// A modport belongs to the same answer, and not merely because it narrows: a
// view also names things of its own (LRM 25.5.4), and two views may give one
// name different storage, so a unit bound through each reaches a different
// place under the same spelling.
auto InterfacePortInput(
    const slang::ast::PortConnection& connection,
    const UnitNamingPolicy& policy) -> UnitKeyInput {
  const auto [instances, modport] =
      ConnectedInterfaceOf(connection.getIfaceConn());
  FixedInterface fixed{
      .specializations = {},
      .taken = {},
      .modport =
          modport == nullptr ? std::string{} : std::string{modport->name}};
  fixed.taken.reserve(instances.size());
  for (const slang::ast::InstanceSymbol* instance : instances) {
    std::string named = policy.NameOf(*instance);
    const auto known = std::ranges::find(fixed.specializations, named);
    fixed.taken.push_back(
        static_cast<std::uint32_t>(known - fixed.specializations.begin()));
    if (known == fixed.specializations.end()) {
      fixed.specializations.push_back(std::move(named));
    }
  }
  return UnitKeyInput{
      .name = std::string{connection.port.name}, .kind = std::move(fixed)};
}

// Whether `symbol` is the scope a compilation unit is: a package (LRM 26), a
// design element's body (LRM 23.2, 25), or the `$unit` file-set scope (LRM
// 3.12.1).
auto IsCompilationUnit(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::Package ||
         symbol.kind == slang::ast::SymbolKind::InstanceBody ||
         symbol.kind == slang::ast::SymbolKind::CompilationUnit;
}

// The bytes a key folds to. Every part is written with its own delimiter, so
// two keys that differ anywhere differ here; nothing rests on this being read
// back, and nothing compares keys through it -- a key answers that itself.
auto KeyBytes(const UnitKey& key) -> std::string {
  std::string bytes;
  for (const UnitKeyInput& input : key.inputs) {
    bytes += input.name;
    bytes += '=';
    bytes += std::visit(
        Overloaded{
            [](const FixedValue& v) { return v.value; },
            [](const FixedType& v) { return v.type; },
            [](const FixedInterface& v) {
              std::string at = "<";
              for (const std::string& named : v.specializations) {
                at += named;
                at += ',';
              }
              at += '|';
              for (const std::uint32_t taken : v.taken) {
                at += std::format("{},", taken);
              }
              return std::format("{}|{}>", at, v.modport);
            },
            [](const LandsIn& v) {
              return std::format(
                  "<in {}:{} {}>", v.specialization.size(), v.specialization,
                  DefPathIdentity(v.scope));
            },
            [](const LandsBack& v) {
              return std::format(
                  "<back {} {}>", v.levels, DefPathIdentity(v.blocks));
            },
            [](const BindInstantiation& v) {
              return std::format("<bound by {}>", v.directive);
            },
            [](const BoundToCell& v) {
              return std::format("<cell {}>", v.cell);
            }},
        input.kind);
    bytes += ';';
  }
  return bytes;
}

// Where a name that left an instance lands, as one input to that instance's
// key: the specialization holding the scope it lands in and the scope's path
// there, or where that scope stands relative to an instance whose key is being
// worked out.
auto LandingOf(const slang::ast::Scope& scope, const UnitNamingPolicy& policy)
    -> UnitKeyInputKind {
  support::DefPath blocks;
  const slang::ast::Symbol* at = &scope.asSymbol();
  while (at->kind != slang::ast::SymbolKind::InstanceBody) {
    if (const auto* block = at->as_if<slang::ast::GenerateBlockSymbol>()) {
      blocks.data.emplace_back(policy.BlockStepOf(*block));
    }
    at = &at->getHierarchicalParent()->asSymbol();
  }
  if (const auto levels =
          policy.Keys().LevelsOutTo(at->as<slang::ast::InstanceBodySymbol>())) {
    std::ranges::reverse(blocks.data);
    return LandsBack{.levels = *levels, .blocks = std::move(blocks)};
  }
  return LandsIn{
      .specialization = policy.NameOf(
          InstantiationOf(at->as<slang::ast::InstanceBodySymbol>())),
      .scope = DefPathOf(scope.asSymbol(), policy)};
}

// Something written elsewhere about the instance at `path`, as a part of the
// key of whatever holds that instance.
auto OverrideInput(
    const std::string& path, const OverrideEffect& effect,
    const UnitNamingPolicy& policy) -> UnitKeyInput {
  return std::visit(
      Overloaded{
          [&](const ParameterGivenElsewhere& given) {
            UnitKeyInput input = ParameterInput(*given.parameter, policy);
            input.name = std::format("{}.{}", path, input.name);
            return input;
          },
          [&](const InsertedByBind& bound) {
            return UnitKeyInput{
                .name = path,
                .kind = BindInstantiation{.directive = bound.directive}};
          },
          [&](const CellChosenByConfiguration& chosen) {
            return UnitKeyInput{
                .name = path,
                .kind = BoundToCell{.cell = CellName(*chosen.cell)}};
          }},
      effect);
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

namespace {

// Which design element `inst` is an instance of. A name alone does not say: a
// library cell is found by its library and its name (LRM 33.2.1), and a module
// declared inside another by the module declaring it and its name (LRM 23.4).
// The nested one reads the parameters of the element declaring it (LRM 23.9),
// so it is named after the unit that element's instance is, the way everything
// else a unit declares is.
auto DefinitionName(
    const slang::ast::InstanceSymbol& inst, const UnitNamingPolicy& policy)
    -> std::string {
  const slang::ast::DefinitionSymbol& definition = inst.getDefinition();
  if (IsLibraryCell(definition)) return CellName(definition);

  // A nested declaration is one of the body declaring it, and that body is one
  // the instance stands in, since the name is visible nowhere else.
  return std::format(
      "{}::{}", UnitWhileNaming(BodyDeclaring(definition), policy),
      definition.name);
}

// What `inst` is an application of, read off the body it is read through.
auto SpecializationKeyOf(
    const slang::ast::InstanceSymbol& inst, const Specializations& keys)
    -> UnitKey {
  const UnitNamingPolicy& names = keys.Names();
  // Every part may ask for another instance's key, and that key may lead back
  // here: an interface this instance carries may itself carry one that stands
  // inside it (LRM 25.3). So the instance is among those being worked out for
  // the whole of its key.
  keys.EnterKey(inst.body);
  UnitKey key{.definition = DefinitionName(inst, names), .inputs = {}};
  // A local parameter is written from the others by its own declaration, so it
  // names nothing they do not.
  for (const auto* param : inst.body.getParameters()) {
    if (param->isLocalParam()) continue;
    key.inputs.push_back(ParameterInput(param->symbol, names));
  }
  for (const auto* connection : inst.getPortConnections()) {
    if (connection->port.kind == slang::ast::SymbolKind::InterfacePort) {
      key.inputs.push_back(InterfacePortInput(*connection, names));
    }
  }
  const InstanceContext& context = keys.ContextOf(inst);
  // A name is no parameter and declares no name of its own, so it is told
  // apart from its writer's other names by its place among them.
  for (std::size_t written = 0; written < context.climbs.size(); ++written) {
    key.inputs.push_back(
        UnitKeyInput{
            .name = std::format("^{}", written),
            .kind = LandingOf(*context.climbs[written].scope, names)});
  }
  for (const NameLandingBelow& name : context.names_below) {
    key.inputs.push_back(
        UnitKeyInput{
            .name = std::format("{}.^{}", name.path, name.written),
            .kind = LandingOf(*name.scope, names)});
  }
  for (const OverriddenBelow& overridden : context.overridden_below) {
    key.inputs.push_back(
        OverrideInput(overridden.path, overridden.effect, names));
  }
  keys.LeaveKey();
  return key;
}

}  // namespace

auto SpecializationNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  return Keys().SpecializationNameOf(inst);
}

auto SpecializationNames::BlockStepOf(
    const slang::ast::GenerateBlockSymbol& block) const
    -> support::GenerateBlockStep {
  const std::optional<std::uint64_t> index =
      block.getArrayIndex() == nullptr
          ? std::nullopt
          : std::optional{static_cast<std::uint64_t>(LoopIndexOf(block))};
  return support::GenerateBlockStep{
      .label = GenerateBlockLabel(block),
      .disambiguator = LabelDisambiguatorOf(block),
      .arguments = index};
}

auto Specializations::AnswersNow(const KeptKey& kept) const -> bool {
  return std::ranges::none_of(
      chain_, [&](const slang::ast::InstanceBodySymbol* body) {
        return kept.depends_on.contains(body);
      });
}

auto Specializations::Worked(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  if (const auto kept = keys_.find(&inst);
      kept != keys_.end() && AnswersNow(kept->second)) {
    for (KeyInProgress& asking : keys_in_progress_) {
      asking.depends_on.insert(
          kept->second.depends_on.begin(), kept->second.depends_on.end());
    }
    return kept->second.name;
  }
  keys_in_progress_.push_back(
      KeyInProgress{
          .chain_at_start = chain_.size(),
          .met_nothing_outside = true,
          .depends_on = {}});
  UnitKey key = SpecializationKeyOf(inst, *this);
  KeyInProgress worked_out = std::move(keys_in_progress_.back());
  keys_in_progress_.pop_back();
  std::string name = UnitName(key);
  // A key asked while another is being worked out can differ from the one the
  // instance has alone, where a name it writes lands in an instance still
  // being worked out, so it is kept only when nothing it met lay outside
  // itself; it answers a later asking only while none of the instances it
  // looked for and did not find is being worked out.
  if (!worked_out.met_nothing_outside) return name;
  if (const auto folded = folded_.find(name); folded == folded_.end()) {
    folded_.emplace(name, key);
  } else if (folded->second != key) {
    throw InternalError(
        "Specializations: two keys reached one name, so the name no longer "
        "tells the specializations apart");
  }
  keys_.insert_or_assign(
      &inst, KeptKey{
                 .key = std::move(key),
                 .name = name,
                 .depends_on = std::move(worked_out.depends_on)});
  return name;
}

auto Specializations::FixesNothing(const slang::ast::InstanceSymbol& inst) const
    -> bool {
  Worked(inst);
  return keys_.at(&inst).key.inputs.empty();
}

auto Specializations::DefinitionOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  Worked(inst);
  return keys_.at(&inst).key.definition;
}

auto Specializations::SpecializationNameOf(
    const slang::ast::InstanceSymbol& inst) const -> std::string {
  return Worked(inst);
}

auto Specializations::ChecksItself() const -> bool {
  switch (bodies_read_) {
    case support::BodiesRead::kOnePerName:
      return false;
    case support::BodiesRead::kEveryInstance:
      return true;
  }
  return false;
}

auto Specializations::HasBodyOfItsOwn(
    const slang::ast::InstanceSymbol& inst) const -> bool {
  const slang::ast::InstanceBodySymbol* shared = inst.getCanonicalBody();
  if (ChecksItself() || shared == nullptr || read_anyway_.contains(&inst)) {
    return true;
  }
  // What is written elsewhere reaches one instance and not another (LRM
  // 23.10.1, 23.11, 33.4), so where the shared body has any of it below, this
  // instance is read through its own body like one the front end elaborated.
  return !ContextOf(InstantiationOf(*shared)).overridden_below.empty();
}

void Specializations::ReadBodyOf(const slang::ast::InstanceSymbol& inst) const {
  if (!read_anyway_.insert(&inst).second) return;
  per_instance_.erase(&inst);
  context_.erase(&inst);
  keys_.erase(&inst);
}

auto Specializations::ContextOf(const slang::ast::InstanceSymbol& inst) const
    -> const InstanceContext& {
  return InstanceContextOf(inst, context_, ReadThroughItsOwnBody());
}

auto Specializations::ReadThroughItsOwnBody() const -> HasBodyOfItsOwnFn {
  return [this](const slang::ast::InstanceSymbol& inst) {
    return HasBodyOfItsOwn(inst);
  };
}

void Specializations::EnterKey(
    const slang::ast::InstanceBodySymbol& body) const {
  chain_.push_back(&body);
}

void Specializations::LeaveKey() const {
  chain_.pop_back();
}

auto Specializations::LevelsOutTo(const slang::ast::InstanceBodySymbol& body)
    const -> std::optional<std::uint32_t> {
  for (std::size_t out = 0; out < chain_.size(); ++out) {
    const std::size_t at = chain_.size() - 1 - out;
    if (chain_[at] == &body) {
      for (KeyInProgress& asking : keys_in_progress_) {
        if (asking.chain_at_start > at) asking.met_nothing_outside = false;
      }
      return static_cast<std::uint32_t>(out);
    }
  }
  for (KeyInProgress& asking : keys_in_progress_) {
    asking.depends_on.insert(&body);
  }
  return std::nullopt;
}

auto DeclaresAClass(const slang::ast::Scope& scope) -> bool {
  for (const slang::ast::Symbol& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::ClassType ||
        member.kind == slang::ast::SymbolKind::GenericClassDef ||
        member.kind == slang::ast::SymbolKind::CovergroupType) {
      return true;
    }
    if (const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>()) {
      if (!block->isUninstantiated && DeclaresAClass(*block)) return true;
    } else if (
        const auto* array =
            member.as_if<slang::ast::GenerateBlockArraySymbol>()) {
      for (const slang::ast::GenerateBlockSymbol* entry : array->entries) {
        if (DeclaresAClass(*entry)) return true;
      }
    }
  }
  return false;
}

auto ArgumentsDigest(const UnitKey& key) -> std::optional<std::uint64_t> {
  if (key.inputs.empty()) return std::nullopt;
  return NameDigest(KeyBytes(key));
}

auto UnitName(const UnitKey& key) -> std::string {
  return WithDigest(key.definition, ArgumentsDigest(key));
}

namespace {

// The key of the class `cls` under `definition`, the name its declaration is
// told from others by wherever the key is used.
auto ClassKeyUnder(
    std::string definition, const slang::ast::ClassType& cls,
    const UnitNamingPolicy& policy) -> UnitKey {
  UnitKey key{.definition = std::move(definition), .inputs = {}};
  if (cls.genericClass == nullptr) {
    return key;
  }
  for (const auto* sym : cls.genericParameters) {
    key.inputs.push_back(ParameterInput(*sym, policy));
  }
  return key;
}

// A class is declared in a design element, a package, a generate block or
// another class (LRM 8.3, A.1.4, A.1.9), so no other scope stands above one.
[[noreturn]] void ThrowClassDeclaredIn(std::string_view what) {
  throw InternalError(
      std::format("UnitKeyOf: a class is declared inside {}", what));
}

}  // namespace

auto UnitKeyOf(const slang::ast::ClassType& cls, const UnitNamingPolicy& policy)
    -> UnitKey {
  // The key names a class inside its compilation unit by one string, and the
  // source name alone does not: SystemVerilog scopes the declaration, so
  // sibling generate blocks may each declare the same one (LRM 27.3), and so
  // may two classes (LRM 8.23).
  const support::DefPath path = DefPathOf(cls, policy);
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
              return WithDigest(std::move(text), block.arguments);
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
  return ClassKeyUnder(std::move(definition), cls, policy);
}

auto UnitName(const slang::ast::ClassType& cls, const UnitNamingPolicy& policy)
    -> std::string {
  return UnitName(UnitKeyOf(cls, policy));
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
    const slang::ast::Symbol& unit, const UnitNamingPolicy& policy)
    -> std::string {
  using slang::ast::SymbolKind;
  if (unit.kind == SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == SymbolKind::InstanceBody) {
    return policy.NameOf(
        InstantiationOf(unit.as<slang::ast::InstanceBodySymbol>()));
  }
  if (const auto* spec = unit.as_if<slang::ast::ClassType>()) {
    return std::format(
        "{}__{}", CompilationUnitName(DeclaringCompilationUnit(*spec), policy),
        UnitName(*spec, policy));
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
auto StepOf(const slang::ast::Symbol& scope, const UnitNamingPolicy& policy)
    -> std::optional<support::DefPathData> {
  using slang::ast::SymbolKind;
  if (scope.kind == SymbolKind::GenerateBlockArray ||
      scope.kind == SymbolKind::AnonymousProgram) {
    return std::nullopt;
  }
  if (const auto* block = scope.as_if<slang::ast::GenerateBlockSymbol>()) {
    return policy.BlockStepOf(*block);
  }
  // A class is told from the others of its scope by its name and, for a
  // specialization of a generic class, what its parameters were bound to
  // (LRM 8.25).
  if (const auto* cls = scope.as_if<slang::ast::ClassType>()) {
    return support::ClassStep{
        .name = std::string{cls->name},
        .arguments = ArgumentsDigest(
            ClassKeyUnder(std::string{cls->name}, *cls, policy))};
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

auto ProceduralStepOf(const slang::ast::Symbol& scope) -> support::DefPathData {
  if (scope.kind == slang::ast::SymbolKind::Subroutine) {
    return support::SubroutineStep{.name = std::string{scope.name}};
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
    const slang::ast::Symbol& declaration, const UnitNamingPolicy& policy)
    -> support::DefPath {
  support::DefPath path;
  for (const slang::ast::Symbol* at = &declaration; !IsCompilationUnit(*at);) {
    if (auto step = StepOf(*at, policy)) {
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
