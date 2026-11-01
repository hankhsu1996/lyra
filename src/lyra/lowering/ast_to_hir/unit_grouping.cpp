#include "lyra/lowering/ast_to_hir/unit_grouping.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/structure_bytes.hpp"
#include "lyra/lowering/ast_to_hir/canonical_body.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/lowering/ast_to_hir/outside_reach.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A digest as text.
auto Hex(std::uint64_t digest) -> std::string {
  return std::format("{:016x}", digest);
}

// The instance holding the scope `symbol` stands in, or nothing where it stands
// in none: a top's own parent is the design's root.
auto Enclosing(const slang::ast::Symbol& symbol)
    -> const slang::ast::InstanceSymbol* {
  for (const slang::ast::Scope* level = symbol.getParentScope();
       level != nullptr; level = level->asSymbol().getParentScope()) {
    if (const auto* body =
            level->asSymbol().as_if<slang::ast::InstanceBodySymbol>()) {
      return body->parentInstance;
    }
  }
  return nullptr;
}

// Whether `inst` is `root` or stands somewhere inside it.
auto Inside(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::InstanceSymbol& root) -> bool {
  for (const slang::ast::InstanceSymbol* at = &inst; at != nullptr;
       at = Enclosing(*at)) {
    if (at == &root) return true;
  }
  return false;
}

// The way down from `root` to `inst`, which stands inside it.
auto RouteFrom(
    const slang::ast::InstanceSymbol& root,
    const slang::ast::InstanceSymbol& inst) -> std::string {
  const std::string from = root.getHierarchicalPath();
  std::string to = inst.getHierarchicalPath();
  if (to.starts_with(from)) to.erase(0, from.size());
  return to;
}

// The instances declaring a type `type` names, by its declaration or through
// the elements it holds.
void InstancesDeclaring(
    const slang::ast::Type& type,
    std::vector<const slang::ast::InstanceSymbol*>& out) {
  const slang::ast::Type& canonical = type.getCanonicalType();
  // A type the source declares is one type per declaration (LRM 6.22.1, 8.3),
  // so it names the instance declaring it.
  if (canonical.isClass() || canonical.isCovergroup() || canonical.isEnum() ||
      canonical.isStruct() || canonical.isPackedUnion() ||
      canonical.isUnpackedUnion()) {
    const slang::ast::Symbol& home = UnitHomeOf(canonical);
    if (const auto* body = home.as_if<slang::ast::InstanceBodySymbol>()) {
      out.push_back(&InstantiationOf(*body));
    }
    return;
  }
  if (canonical.isVirtualInterface()) {
    out.push_back(&canonical.as<slang::ast::VirtualInterfaceType>().iface);
    return;
  }
  if (canonical.isAssociativeArray()) {
    const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
    if (assoc.indexType != nullptr) InstancesDeclaring(*assoc.indexType, out);
    InstancesDeclaring(assoc.elementType, out);
    return;
  }
  if (const slang::ast::Type* element = canonical.getArrayElementType()) {
    InstancesDeclaring(*element, out);
  }
}

class Grouping;

// The naming one lowering is described in while units are found: the
// instance being described and every other instance of its lowering by one
// mark, an instance it holds by what that one publishes, one outside it that
// its text reaches by where it stands among those, and any other by its unit.
class GroupingNames final : public InstanceNames {
 public:
  GroupingNames(
      Grouping& grouping, const slang::ast::InstanceSymbol& described,
      const std::vector<const slang::ast::InstanceSymbol*>& reached,
      std::set<std::size_t>& named)
      : grouping_(&grouping),
        described_(&described),
        reached_(&reached),
        named_(&named) {
  }

  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;

  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override {
    return PathStepOf(block);
  }

  // Notes that what was declared names the reached instance at `reached`.
  void NoteNamed(std::size_t reached) const {
    named_->insert(reached);
  }

 private:
  Grouping* grouping_;
  const slang::ast::InstanceSymbol* described_;
  const std::vector<const slang::ast::InstanceSymbol*>* reached_;
  // Which of the reached instances what was declared names, by position, held
  // by the description this naming serves.
  std::set<std::size_t>* named_;
};

// Every distinct thing of one kind grouping wrote, each by its digest and held
// against the whole of what was written for it: two are alike exactly where
// everything written of them is equal, and two that differ but share a digest
// cannot be named apart, which is reported rather than taken for one. Each is
// written with what it names by digest, and each of those was held to its own
// text in turn, so a digest stands for one text all the way down.
class Interned {
 public:
  explicit Interned(std::string_view kind) : kind_(kind) {
  }

  auto Of(std::string written) -> std::uint64_t {
    const std::uint64_t digest = NameDigest(written);
    const auto [kept, fresh] = written_.try_emplace(digest, std::move(written));
    if (!fresh && kept->second != written) {
      throw InternalError(
          std::format(
              "Grouping: two {} that differ reached one digest, so the digest "
              "no longer tells them apart -- please report this as a bug",
              kind_));
    }
    return digest;
  }

 private:
  std::string_view kind_;
  std::unordered_map<std::uint64_t, std::string> written_;
};

// The body a lowering is lowered from, which is what one is identified by
// while units are found: an in-memory identity that never crosses a unit.
using Body = const slang::ast::InstanceBodySymbol*;

class Grouping {
 public:
  Grouping(const ElaboratedInstances& instances, const DeclareLowering& declare)
      : instances_(&instances), declare_(&declare) {
  }

  // What the lowering of `inst` publishes, every reference in it written by
  // what it reaches.
  auto StructureOf(const slang::ast::InstanceSymbol& inst) -> std::uint64_t {
    const Body body = &CanonicalBodyOf(inst);
    if (const auto kept = structures_.find(body); kept != structures_.end()) {
      return kept->second;
    }
    const slang::ast::InstanceSymbol& described = InstantiationOf(*body);
    being_described_.push_back(body);
    const std::vector<const slang::ast::InstanceSymbol*> reached =
        ReachedFrom(described);
    std::set<std::size_t> named;
    const GroupingNames names(*this, described, reached, named);
    const std::optional<hir::UnitSignature> declared =
        (*declare_)(described, names);
    // One that fails to declare stops the compile once grouping returns, and
    // is a unit of its own until then.
    if (!declared.has_value()) {
      being_described_.pop_back();
      named_.emplace(body, std::set<std::size_t>{});
      return structures_
          .emplace(
              body, structures_found_.Of("!" + described.getHierarchicalPath()))
          .first->second;
    }
    const hir::UnitSignature& signature = *declared;

    // A block's class, written by the digest of what it publishes.
    std::map<support::DefPath, std::uint64_t> blocks;
    // The block being written. A path inside it is written from it, and one
    // reaching a block around it by how far out that block stands, so where
    // the block stands says nothing of what it holds.
    const support::DefPath* writing = nullptr;
    const hir::ReferenceBytes reference =
        [&](const std::string& unit_name,
            const support::DefPath& path) -> std::string {
      if (unit_name != signature.unit_name) {
        return unit_name + "|" + support::DefPathIdentity(path);
      }
      std::string out = "s|";
      support::DefPath prefix;
      std::size_t from = 0;
      if (writing != nullptr) {
        const auto apart = std::ranges::mismatch(writing->data, path.data);
        const auto common =
            static_cast<std::size_t>(apart.in1 - writing->data.begin());
        out += std::format("up{};", writing->data.size() - common);
        prefix.data.assign(path.data.begin(), apart.in2);
        from = common;
      }
      for (const support::DefPathData& step :
           std::span{path.data}.subspan(from)) {
        prefix.data.push_back(step);
        const auto block = blocks.find(prefix);
        if (std::holds_alternative<support::GenerateBlockStep>(step) &&
            block != blocks.end()) {
          out += std::format("b{};", Hex(block->second));
        } else {
          out += support::DefPathIdentity(support::DefPath{.data = {step}});
        }
      }
      return out;
    };
    if (const auto* element =
            std::get_if<hir::PublishedDesignElement>(&signature.unit)) {
      std::vector<const hir::ScopeClassSignature*> inner_first;
      for (const hir::ScopeClassSignature& block : element->blocks) {
        inner_first.push_back(&block);
      }
      std::ranges::stable_sort(
          inner_first, std::ranges::greater{},
          [](const hir::ScopeClassSignature* block) {
            return block->class_path.data.size();
          });
      for (const hir::ScopeClassSignature* block : inner_first) {
        writing = &block->class_path;
        std::string written =
            hir::BlockClassBytes(signature, *block, reference);
        writing = nullptr;
        blocks.insert_or_assign(
            block->class_path, blocks_found_.Of(std::move(written)));
      }
    }
    for (const auto& [path, digest] : blocks) {
      block_class_of_.insert_or_assign(std::pair{body, path}, digest);
    }

    // A unit is a unit of one definition: two cells of one name in two
    // libraries (LRM 33.2.1) may publish alike and are still two.
    std::string written = DefinitionNameOf(described) + "|" +
                          hir::StructureBytes(signature, reference);
    // Where what each instance it holds reaches outside itself lands: inside
    // this one, at a place, or outside it, among what this one reaches.
    for (const slang::ast::InstanceSymbol* child :
         InstancesHeldIn(described.body)) {
      written += std::format("|{}", RouteFrom(described, *child));
      for (const slang::ast::InstanceSymbol* target : ReachedFrom(*child)) {
        if (Inside(*target, described)) {
          written += std::format("=in:{};", RouteFrom(described, *target));
          continue;
        }
        const auto among = std::ranges::find(reached, target);
        if (among != reached.end()) {
          const auto at = static_cast<std::size_t>(among - reached.begin());
          names.NoteNamed(at);
          written += std::format("=x{};", at);
          continue;
        }
        written += std::format("=c:{};", UnitOf(*target));
      }
    }
    being_described_.pop_back();
    named_.emplace(body, std::move(named));
    return structures_.emplace(body, structures_found_.Of(std::move(written)))
        .first->second;
  }

  // The unit the lowering of `inst` is, as the digest of what it publishes
  // and the unit of each instance outside it that what it publishes names --
  // what its code alone reaches is the code's, and no referrer compiles
  // against it. Every lowering on a cycle of units turning on one another is
  // written by its own place instead, which keeps it apart from every other
  // and never takes two for one.
  auto UnitOf(const slang::ast::InstanceSymbol& inst) -> std::string {
    const Body body = &CanonicalBodyOf(inst);
    if (const auto kept = units_.find(body); kept != units_.end()) {
      return kept->second;
    }
    const slang::ast::InstanceSymbol& described = InstantiationOf(*body);
    const std::string own = "~" + described.getHierarchicalPath();
    const auto on_the_way = [&](const std::vector<Body>& stack) {
      return std::ranges::find(stack, body);
    };
    if (const auto from = on_the_way(being_classed_);
        from != being_classed_.cend()) {
      cyclic_.insert(from, being_classed_.cend());
      return own;
    }
    if (on_the_way(being_described_) != being_described_.end()) {
      cyclic_.insert(body);
      return own;
    }
    being_classed_.push_back(body);
    std::string written = Hex(StructureOf(described));
    const std::vector<const slang::ast::InstanceSymbol*> reached =
        ReachedFrom(described);
    for (const std::size_t at : named_.at(body)) {
      written += std::format("|{}:{}", at, UnitOf(*reached.at(at)));
    }
    being_classed_.pop_back();
    if (cyclic_.contains(body)) written = own;
    return units_.emplace(body, Hex(units_found_.Of(std::move(written))))
        .first->second;
  }

  // The instances outside `inst` that what the design fixed for it reaches:
  // the instances declaring a type a parameter is fixed to (LRM 6.20.3), the
  // interfaces its interface ports are bound to (LRM 25.3), the instances the
  // names it and those below it write land in (LRM 23.8), and the instance
  // declaring its definition (LRM 23.4) -- each once, in that order.
  auto ReachedFrom(const slang::ast::InstanceSymbol& inst)
      -> std::vector<const slang::ast::InstanceSymbol*> {
    std::vector<const slang::ast::InstanceSymbol*> found;
    for (const auto* param : inst.body.getParameters()) {
      if (const auto* type =
              param->symbol.as_if<slang::ast::TypeParameterSymbol>()) {
        InstancesDeclaring(type->targetType.getType(), found);
      }
    }
    for (const auto* connection : inst.getPortConnections()) {
      if (connection->port.kind != slang::ast::SymbolKind::InterfacePort) {
        continue;
      }
      for (const slang::ast::InstanceSymbol* bound :
           ConnectedInterfaceOf(connection->getIfaceConn()).instances) {
        found.push_back(bound);
      }
    }
    const OutsideReach& context = instances_->OutsideReachOf(inst);
    const auto lands =
        [&](const slang::ast::Scope& landed,
            std::span<const slang::ast::Scope* const> blocks_passed) {
          if (const auto* at = InstanceAt(landed)) found.push_back(at);
          for (const slang::ast::Scope* passed : blocks_passed) {
            if (const auto* at = InstanceAt(*passed)) found.push_back(at);
          }
        };
    for (const NameUse& use : context.uses) {
      if (use.landing.has_value()) {
        lands(*use.landing->scope, use.landing->blocks_passed);
      }
    }
    for (const ClimbAnchor& name : context.names_below) {
      lands(*name.scope, name.blocks_passed);
    }
    const slang::ast::DefinitionSymbol& definition = inst.getDefinition();
    if (!IsLibraryCell(definition)) {
      const slang::ast::InstanceBodySymbol& declaring =
          BodyDeclaring(definition);
      if (declaring.parentInstance != nullptr) {
        found.push_back(declaring.parentInstance);
      }
    }
    std::vector<const slang::ast::InstanceSymbol*> reached;
    for (const slang::ast::InstanceSymbol* target : found) {
      if (Inside(*target, inst) || std::ranges::contains(reached, target)) {
        continue;
      }
      reached.push_back(target);
    }
    return reached;
  }

  // The class of each block of every lowering described, by the body and
  // where the block stands there.
  [[nodiscard]] auto Blocks() const
      -> const std::map<std::pair<Body, support::DefPath>, std::uint64_t>& {
    return block_class_of_;
  }

  // The instance of every lowering met, the ones grouping reached on its way
  // included, in the order of their places.
  [[nodiscard]] auto Classified() const
      -> std::vector<const slang::ast::InstanceSymbol*> {
    std::vector<const slang::ast::InstanceSymbol*> out;
    out.reserve(units_.size());
    for (const auto& [body, _] : units_) {
      out.push_back(&InstantiationOf(*body));
    }
    std::ranges::sort(out, {}, [](const slang::ast::InstanceSymbol* inst) {
      return inst->getHierarchicalPath();
    });
    return out;
  }

 private:
  static auto InstanceAt(const slang::ast::Scope& scope)
      -> const slang::ast::InstanceSymbol* {
    if (const auto* body =
            scope.asSymbol().as_if<slang::ast::InstanceBodySymbol>()) {
      return body->parentInstance;
    }
    return Enclosing(scope.asSymbol());
  }

  const ElaboratedInstances* instances_;
  const DeclareLowering* declare_;
  Interned structures_found_{"published structures"};
  Interned units_found_{"classes"};
  Interned blocks_found_{"block classes"};
  std::unordered_map<Body, std::uint64_t> structures_;
  // Which reached instances each lowering's structure names.
  std::unordered_map<Body, std::set<std::size_t>> named_;
  std::unordered_map<Body, std::string> units_;
  // The lowerings being described and those whose unit is being worked out,
  // outermost first.
  std::vector<Body> being_described_;
  std::vector<Body> being_classed_;
  std::unordered_set<Body> cyclic_;
  std::map<std::pair<Body, support::DefPath>, std::uint64_t> block_class_of_;
};

auto GroupingNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  if (&CanonicalBodyOf(inst) == &CanonicalBodyOf(*described_)) return "s";
  if (Inside(inst, *described_))
    return "r:" + Hex(grouping_->StructureOf(inst));
  const auto among = std::ranges::find(*reached_, &inst);
  if (among != reached_->end()) {
    const auto at = static_cast<std::size_t>(among - reached_->begin());
    NoteNamed(at);
    return std::format("x{}", at);
  }
  return "c:" + grouping_->UnitOf(inst);
}

}  // namespace

auto UnitNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  const auto unit = unit_of_.find(&CanonicalBodyOf(inst));
  if (unit == unit_of_.end()) {
    throw InternalError(
        std::format(
            "UnitNames: '{}' was grouped into no unit",
            inst.getHierarchicalPath()));
  }
  return unit->second;
}

auto UnitNames::BlockStepOf(const slang::ast::GenerateBlockSymbol& block) const
    -> support::GenerateBlockStep {
  support::DefPath path;
  const slang::ast::Symbol* at = &block;
  while (at->kind != slang::ast::SymbolKind::InstanceBody) {
    if (const auto* enclosing = at->as_if<slang::ast::GenerateBlockSymbol>()) {
      path.data.emplace_back(PathStepOf(*enclosing));
    }
    at = &at->getParentScope()->asSymbol();
  }
  std::ranges::reverse(path.data);
  // A block of an instance lowered with another stands at the same path in
  // that one's body.
  const auto* body = &CanonicalBodyOf(
      InstantiationOf(at->as<slang::ast::InstanceBodySymbol>()));
  const auto found = block_class_of_.find(std::pair{body, path});
  if (found == block_class_of_.end()) {
    throw InternalError(
        std::format(
            "UnitNames: block '{}' of '{}' was published by no lowering",
            block.name, InstantiationOf(*body).getHierarchicalPath()));
  }
  support::GenerateBlockStep step =
      std::get<support::GenerateBlockStep>(path.data.back());
  step.distinguisher = found->second;
  return step;
}

auto GroupLowerings(
    const ElaboratedInstances& instances,
    const std::vector<const slang::ast::InstanceSymbol*>& lowered,
    const DeclareLowering& declare) -> std::unique_ptr<UnitNames> {
  Grouping grouping(instances, declare);
  for (const slang::ast::InstanceSymbol* inst : lowered) {
    grouping.UnitOf(*inst);
  }
  // A unit is named by its definition alone where the design holds no other
  // unit of that definition, and by the digest of what it publishes
  // otherwise. Either way the name is a function of the design's content and
  // never of the order it is read in.
  std::map<std::string, std::vector<const slang::ast::InstanceSymbol*>> members;
  std::unordered_map<std::string, std::size_t> units_of_definition;
  for (const slang::ast::InstanceSymbol* inst : grouping.Classified()) {
    auto& of_unit = members[grouping.UnitOf(*inst)];
    if (of_unit.empty()) ++units_of_definition[DefinitionNameOf(*inst)];
    of_unit.push_back(inst);
  }
  std::unordered_map<Body, std::string> unit_of;
  for (const auto& [digest, of_unit] : members) {
    std::string unit = DefinitionNameOf(*of_unit.front());
    if (units_of_definition.at(unit) != 1) unit += "__" + digest;
    for (const slang::ast::InstanceSymbol* inst : of_unit) {
      unit_of.emplace(&CanonicalBodyOf(*inst), unit);
    }
  }
  return std::make_unique<UnitNames>(std::move(unit_of), grouping.Blocks());
}

}  // namespace lyra::lowering::ast_to_hir
