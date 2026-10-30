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

#include <slang/ast/ASTVisitor.h>
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
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/instance_context.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"

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

// The instances directly held by `scope`, wherever its generate blocks and
// instance arrays place them, in the order the source writes them.
void HeldInstances(
    const slang::ast::Scope& scope,
    std::vector<const slang::ast::InstanceSymbol*>& out) {
  for (const slang::ast::Symbol& member : scope.members()) {
    if (const auto* inst = member.as_if<slang::ast::InstanceSymbol>()) {
      out.push_back(inst);
    } else if (
        const auto* array = member.as_if<slang::ast::InstanceArraySymbol>()) {
      HeldInstances(*array, out);
    } else if (
        const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>()) {
      if (!block->isUninstantiated) HeldInstances(*block, out);
    } else if (
        const auto* loop =
            member.as_if<slang::ast::GenerateBlockArraySymbol>()) {
      for (const slang::ast::GenerateBlockSymbol* entry : loop->entries) {
        HeldInstances(*entry, out);
      }
    }
  }
}

// Whether a class or a covergroup `signature` declares stands inside the scope
// published under `path`.
auto DeclaresAClassAt(
    const hir::UnitSignature& signature, const support::DefPath& path) -> bool {
  const auto inside = [&](const support::DefPath& declared) {
    return declared.data.size() > path.data.size() &&
           std::ranges::equal(
               path.data, std::span{declared.data}.first(path.data.size()));
  };
  return std::ranges::any_of(
             signature.classes,
             [&](const hir::ClassDeclaration& cls) {
               return inside(cls.class_path);
             }) ||
         std::ranges::any_of(
             signature.covergroups,
             [&](const hir::PublishedCovergroup& covergroup) {
               return inside(covergroup.path);
             });
}

class Grouping;

// The naming one specialization is described in while classes are found: the
// instance being described and every other instance of its specialization by
// one mark, an instance it holds by what that one publishes, one outside it
// that its key reaches by where it stands among those, and any other by its
// class.
class GroupingNames final : public UnitNamingPolicy {
 public:
  GroupingNames(
      const Specializations& keys, Grouping& grouping,
      const slang::ast::InstanceSymbol& described,
      const std::vector<const slang::ast::InstanceSymbol*>& reached)
      : UnitNamingPolicy(keys, nullptr),
        grouping_(&grouping),
        described_(&described),
        own_(keys.SpecializationNameOf(described)),
        reached_(&reached) {
  }

  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;

  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override {
    return Keys().Names().BlockStepOf(block);
  }

  // Records that what was declared names the reached instance at `reached`.
  void Name(std::size_t reached) const {
    named_.insert(reached);
  }

  [[nodiscard]] auto Named() const -> const std::set<std::size_t>& {
    return named_;
  }

 private:
  Grouping* grouping_;
  const slang::ast::InstanceSymbol* described_;
  std::string own_;
  const std::vector<const slang::ast::InstanceSymbol*>* reached_;
  // Which of the reached instances what was declared names, by position.
  mutable std::set<std::size_t> named_;
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

class Grouping {
 public:
  Grouping(const Specializations& keys, const DeclareUnder& declare)
      : keys_(&keys), declare_(&declare) {
  }

  void Represent(const slang::ast::InstanceSymbol& inst) {
    representative_.try_emplace(keys_->SpecializationNameOf(inst), &inst);
  }

  // What the specialization of `inst` publishes, every reference in it written
  // by what it reaches.
  auto RelOf(const slang::ast::InstanceSymbol& inst) -> std::uint64_t {
    const std::string specialization = keys_->SpecializationNameOf(inst);
    if (const auto kept = rels_.find(specialization); kept != rels_.end()) {
      return kept->second;
    }
    Represent(inst);
    const slang::ast::InstanceSymbol& described =
        *representative_.at(specialization);
    describing_.push_back(specialization);
    const std::vector<const slang::ast::InstanceSymbol*> reached =
        ReachedFrom(described);
    const GroupingNames names(*keys_, *this, described, reached);
    const std::optional<hir::UnitSignature> declared =
        (*declare_)(described, names);
    // One that fails to declare stops the compile once grouping returns, and
    // is a class of its own until then.
    if (!declared.has_value()) {
      describing_.pop_back();
      named_.emplace(specialization, std::set<std::size_t>{});
      return rels_.emplace(specialization, rels_found_.Of("!" + specialization))
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
        return unit_name + "|" + DefPathIdentity(path);
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
          out += DefPathIdentity(support::DefPath{.data = {step}});
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
        // A class a block declares has methods that are the block's code,
        // named by the block's class, so blocks whose code differs cannot be
        // one class: such a block stays where it stands.
        if (DeclaresAClassAt(signature, block->class_path)) {
          written += "|at:" + DefPathIdentity(block->class_path);
        }
        blocks.insert_or_assign(
            block->class_path, blocks_found_.Of(std::move(written)));
      }
    }
    for (const auto& [path, digest] : blocks) {
      block_class_of_.insert_or_assign(std::pair{specialization, path}, digest);
    }

    // A class is a class of one definition: two cells of one name in two
    // libraries (LRM 33.2.1) may publish alike and are still two.
    std::string written = keys_->DefinitionOf(described) + "|" +
                          hir::StructureBytes(signature, reference);
    // The same holds of a design element declaring a class: its methods are
    // the element's code, named by its class, and anything its key holds can
    // change that code, so each specialization of it is a class of its own.
    if (DeclaresAClass(described.body)) written += "|own:" + specialization;
    // Where what each instance it holds reaches outside itself lands: inside
    // this one, at a place, or outside it, among what this one reaches.
    std::vector<const slang::ast::InstanceSymbol*> held;
    HeldInstances(described.body, held);
    for (const slang::ast::InstanceSymbol* child : held) {
      written += std::format("|{}", RouteFrom(described, *child));
      for (const slang::ast::InstanceSymbol* target : ReachedFrom(*child)) {
        if (Inside(*target, described)) {
          written += std::format("=in:{};", RouteFrom(described, *target));
          continue;
        }
        const auto among = std::ranges::find(reached, target);
        if (among != reached.end()) {
          const auto at = static_cast<std::size_t>(among - reached.begin());
          names.Name(at);
          written += std::format("=x{};", at);
          continue;
        }
        written += std::format("=c:{};", ClassOf(*target));
      }
    }
    describing_.pop_back();
    named_.emplace(specialization, names.Named());
    return rels_.emplace(specialization, rels_found_.Of(std::move(written)))
        .first->second;
  }

  // The class the specialization of `inst` is, as the digest of what it
  // publishes and the class of each instance outside it that what it publishes
  // names -- what its code alone reaches is the code's, and no referrer
  // compiles against it. Every specialization on a cycle of classes turning on
  // one another is written by its own name instead, which keeps it apart from
  // every other and never takes two for one.
  auto ClassOf(const slang::ast::InstanceSymbol& inst) -> std::string {
    const std::string specialization = keys_->SpecializationNameOf(inst);
    if (const auto kept = classes_.find(specialization);
        kept != classes_.end()) {
      return kept->second;
    }
    const auto on_the_way = [&](const std::vector<std::string>& stack) {
      return std::ranges::find(stack, specialization);
    };
    if (const auto from = on_the_way(classing_); from != classing_.cend()) {
      cyclic_.insert(from, classing_.cend());
      return "~" + specialization;
    }
    if (on_the_way(describing_) != describing_.end()) {
      cyclic_.insert(specialization);
      return "~" + specialization;
    }
    Represent(inst);
    const slang::ast::InstanceSymbol& described =
        *representative_.at(specialization);
    classing_.push_back(specialization);
    std::string written = Hex(RelOf(described));
    const std::vector<const slang::ast::InstanceSymbol*> reached =
        ReachedFrom(described);
    for (const std::size_t at : named_.at(specialization)) {
      written += std::format("|{}:{}", at, ClassOf(*reached.at(at)));
    }
    classing_.pop_back();
    if (cyclic_.contains(specialization)) written = "~" + specialization;
    return classes_
        .emplace(specialization, Hex(classes_found_.Of(std::move(written))))
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
    const InstanceContext& context = keys_->ContextOf(inst);
    const auto lands =
        [&](const slang::ast::Scope& landed,
            std::span<const slang::ast::Scope* const> blocks_passed) {
          if (const auto* at = InstanceAt(landed)) found.push_back(at);
          for (const slang::ast::Scope* passed : blocks_passed) {
            if (const auto* at = InstanceAt(*passed)) found.push_back(at);
          }
        };
    for (const ClimbAnchor& climb : context.climbs) {
      lands(*climb.scope, climb.blocks_passed);
    }
    for (const NameLandingBelow& name : context.names_below) {
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

  // Whether `inst` is an application of the specialization named `own`.
  [[nodiscard]] auto SameAs(
      const slang::ast::InstanceSymbol& inst, const std::string& own) const
      -> bool {
    return keys_->SpecializationNameOf(inst) == own;
  }

  // The class of each block of every specialization described, by the
  // specialization and where the block stands there.
  [[nodiscard]] auto Blocks() const -> const
      std::map<std::pair<std::string, support::DefPath>, std::uint64_t>& {
    return block_class_of_;
  }

  // One instance of every specialization met, the ones grouping reached on
  // its way included, in the order of their names.
  [[nodiscard]] auto Represented() const
      -> std::vector<const slang::ast::InstanceSymbol*> {
    std::vector<std::pair<std::string, const slang::ast::InstanceSymbol*>>
        sorted(representative_.begin(), representative_.end());
    std::ranges::sort(sorted, {}, &decltype(sorted)::value_type::first);
    std::vector<const slang::ast::InstanceSymbol*> out;
    out.reserve(sorted.size());
    for (const auto& [_, inst] : sorted) out.push_back(inst);
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

  const Specializations* keys_;
  const DeclareUnder* declare_;
  std::unordered_map<std::string, const slang::ast::InstanceSymbol*>
      representative_;
  Interned rels_found_{"specializations"};
  Interned classes_found_{"classes"};
  Interned blocks_found_{"block classes"};
  std::unordered_map<std::string, std::uint64_t> rels_;
  // Which reached instances each specialization's structure names.
  std::unordered_map<std::string, std::set<std::size_t>> named_;
  std::unordered_map<std::string, std::string> classes_;
  // The specializations being described and those whose class is being worked
  // out, outermost first.
  std::vector<std::string> describing_;
  std::vector<std::string> classing_;
  std::unordered_set<std::string> cyclic_;
  std::map<std::pair<std::string, support::DefPath>, std::uint64_t>
      block_class_of_;
};

auto GroupingNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  if (grouping_->SameAs(inst, own_)) return "s";
  if (Inside(inst, *described_)) return "r:" + Hex(grouping_->RelOf(inst));
  const auto among = std::ranges::find(*reached_, &inst);
  if (among != reached_->end()) {
    const auto at = static_cast<std::size_t>(among - reached_->begin());
    Name(at);
    return std::format("x{}", at);
  }
  return "c:" + grouping_->ClassOf(inst);
}

}  // namespace

auto ClassNames::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  const std::string specialization = Keys().SpecializationNameOf(inst);
  const auto unit = unit_of_.find(specialization);
  if (unit == unit_of_.end()) {
    throw InternalError(
        std::format(
            "ClassNames: specialization '{}' was grouped into no class",
            specialization));
  }
  return unit->second;
}

auto ClassNames::BlockStepOf(const slang::ast::GenerateBlockSymbol& block) const
    -> support::GenerateBlockStep {
  support::DefPath path;
  const slang::ast::Symbol* at = &block;
  while (at->kind != slang::ast::SymbolKind::InstanceBody) {
    if (const auto* enclosing = at->as_if<slang::ast::GenerateBlockSymbol>()) {
      path.data.emplace_back(Keys().Names().BlockStepOf(*enclosing));
    }
    at = &at->getParentScope()->asSymbol();
  }
  std::ranges::reverse(path.data);
  const std::string specialization = Keys().SpecializationNameOf(
      InstantiationOf(at->as<slang::ast::InstanceBodySymbol>()));
  const auto found = block_class_of_.find(std::pair{specialization, path});
  if (found == block_class_of_.end()) {
    throw InternalError(
        std::format(
            "ClassNames: block '{}' of '{}' was published by no "
            "specialization",
            block.name, specialization));
  }
  support::GenerateBlockStep step =
      std::get<support::GenerateBlockStep>(path.data.back());
  step.arguments = found->second;
  return step;
}

auto GroupSpecializations(
    const Specializations& keys,
    const std::vector<const slang::ast::InstanceSymbol*>& representatives,
    const DeclareUnder& declare) -> std::unique_ptr<ClassNames> {
  Grouping grouping(keys, declare);
  for (const slang::ast::InstanceSymbol* inst : representatives) {
    grouping.Represent(*inst);
  }
  // A class is named by its definition alone where it is nothing but the one
  // specialization of the definition the design fixed nothing for, and by the
  // digest of what it publishes otherwise, so every specialization of one
  // class has one name.
  std::map<std::string, std::vector<const slang::ast::InstanceSymbol*>> members;
  for (const slang::ast::InstanceSymbol* inst : grouping.Represented()) {
    members[grouping.ClassOf(*inst)].push_back(inst);
  }
  std::unordered_map<std::string, std::string> unit_of;
  std::unordered_map<std::string, std::string> class_named;
  for (const auto& [cls, of_class] : members) {
    const slang::ast::InstanceSymbol& first = *of_class.front();
    std::string unit = keys.DefinitionOf(first);
    if (of_class.size() != 1 || !keys.FixesNothing(first)) unit += "__" + cls;
    if (const auto [kept, fresh] = class_named.try_emplace(unit, cls);
        !fresh && kept->second != cls) {
      throw InternalError(
          std::format(
              "GroupSpecializations: two classes reached the name '{}', so "
              "the name no longer tells them apart",
              unit));
    }
    for (const slang::ast::InstanceSymbol* inst : of_class) {
      unit_of.emplace(keys.SpecializationNameOf(*inst), unit);
    }
  }
  return std::make_unique<ClassNames>(
      keys, std::move(unit_of), grouping.Blocks());
}

}  // namespace lyra::lowering::ast_to_hir
