#pragma once

// Which specializations of the design are one class: those whose published
// structures read alike once every reference in them is written by what it
// reaches rather than by which specialization it names.
//
// A referrer compiles against exactly what a specialization publishes, so the
// class a specialization is, is the class of every specialization publishing
// alike. What it publishes
// names three kinds of other instance, and each is written by what tells two
// such references apart:
//
// - one it holds, by what that one publishes in turn, so classes are found
//   bottom-up;
// - one outside it that its key reaches -- an ancestor declaring a type it is
//   handed, an interface it is bound to, a scope a name lands in -- by where it
//   stands among those, so that two specializations reaching different
//   instances of one class read alike; the classes of those instances are
//   added afterwards, top-down, to make the class;
// - any other, by its class.
//
// A generate block's class is found the same way inside its unit. The class of
// a specialization is named by the digest of all of it, so the name moves only
// where what referrers compile against moves.

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
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/support/def_path.hpp"

namespace slang::ast {
class GenerateBlockSymbol;
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What one specialization publishes, declared from `instance` under `naming`,
// or nothing where it fails to declare.
using DeclareUnder = std::function<std::optional<hir::UnitSignature>(
    const slang::ast::InstanceSymbol& instance,
    const UnitNamingPolicy& naming)>;

// The naming of the stage that lowers bodies: another instance by the class
// its specialization is, and a block by the class it is inside its unit.
class ClassNames final : public UnitNamingPolicy {
 public:
  ClassNames(
      const Specializations& keys,
      std::unordered_map<std::string, std::string> unit_of,
      std::map<std::pair<std::string, support::DefPath>, std::uint64_t>
          block_class_of)
      : UnitNamingPolicy(keys, nullptr),
        unit_of_(std::move(unit_of)),
        block_class_of_(std::move(block_class_of)) {
  }

  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;
  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override;

 private:
  // The unit of each specialization, by the specialization's name.
  std::unordered_map<std::string, std::string> unit_of_;
  // The digest of each generate block's class, by the specialization holding
  // it and where the block stands there.
  std::map<std::pair<std::string, support::DefPath>, std::uint64_t>
      block_class_of_;
};

// The classes of the specializations `representatives` holds -- one instance
// of each, the first the design meets -- each declared through `declare`.
[[nodiscard]] auto GroupSpecializations(
    const Specializations& keys,
    const std::vector<const slang::ast::InstanceSymbol*>& representatives,
    const DeclareUnder& declare) -> std::unique_ptr<ClassNames>;

}  // namespace lyra::lowering::ast_to_hir
