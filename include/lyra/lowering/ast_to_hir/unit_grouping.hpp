#pragma once

// Which lowerings of the design are one unit: those whose published
// structures read alike once every reference in them is written by what it
// reaches rather than by which instance it names.
//
// A referrer compiles against exactly what an instance publishes, so the unit
// a lowering is, is the unit of every lowering publishing alike. What it
// publishes names three kinds of other instance, and each is written by what
// tells two such references apart:
//
// - one it holds, by what that one publishes in turn, so units are found
//   bottom-up;
// - one outside it that its text reaches -- an ancestor declaring a type it is
//   handed, an interface it is bound to, a scope a name lands in -- by where it
//   stands among those, so that two lowerings reaching different instances of
//   one unit read alike; the units of those instances are added afterwards,
//   top-down, to make the unit;
// - any other, by its unit.
//
// A generate block's class is found the same way inside its unit. A unit is
// named by its definition, with the digest of all of it where the definition
// has more than one unit, so the name moves only where what referrers compile
// against moves.

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/elaborated_instances.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/support/def_path.hpp"

namespace slang::ast {
class GenerateBlockSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What one lowering publishes, declared from `instance` under `names`, or
// nothing where it fails to declare.
using DeclareLowering = std::function<std::optional<hir::UnitSignature>(
    const slang::ast::InstanceSymbol& instance, const InstanceNames& names)>;

// The naming of the stage that lowers bodies: another instance by the unit its
// lowering is, and a block by the class it is inside its unit.
class UnitNames final : public InstanceNames {
 public:
  UnitNames(
      std::unordered_map<const slang::ast::InstanceBodySymbol*, std::string>
          unit_of,
      std::map<
          std::pair<const slang::ast::InstanceBodySymbol*, support::DefPath>,
          std::uint64_t>
          block_class_of)
      : unit_of_(std::move(unit_of)),
        block_class_of_(std::move(block_class_of)) {
  }

  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;
  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override;

 private:
  // The unit of each lowering, by the body it is lowered from.
  std::unordered_map<const slang::ast::InstanceBodySymbol*, std::string>
      unit_of_;
  // The digest of each generate block's class, by the body holding it and
  // where the block stands there.
  std::map<
      std::pair<const slang::ast::InstanceBodySymbol*, support::DefPath>,
      std::uint64_t>
      block_class_of_;
};

// The units of the lowerings `lowered` holds -- the instance each is lowered
// for -- each declared through `declare`.
[[nodiscard]] auto GroupLowerings(
    const ElaboratedInstances& instances,
    const std::vector<const slang::ast::InstanceSymbol*>& lowered,
    const DeclareLowering& declare) -> std::unique_ptr<UnitNames>;

}  // namespace lyra::lowering::ast_to_hir
