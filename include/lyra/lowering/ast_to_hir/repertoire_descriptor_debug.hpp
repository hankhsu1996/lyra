#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <utility>

#include "lyra/lowering/ast_to_hir/repertoire_descriptor.hpp"

namespace slang::ast {
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Debug-only view for dump/test output. Maps declaration artifacts to
// human-readable names. Built alongside the descriptor in one pass;
// not part of semantic identity.
//
// Lookup is O(log n) via sorted map keyed on (coord, local_ordinal).
struct RepertoireDebugView {
  struct Key {
    RepertoireCoord coord;
    uint32_t local_ordinal;

    auto operator<(const Key& other) const -> bool;
  };

  std::map<Key, std::string> decl_names;
};

// Builds both the semantic descriptor and debug view in one pass.
// Use this in test/debug contexts where dump output needs declaration names.
auto BuildDefinitionRepertoireDescWithDebugView(
    const slang::ast::InstanceBodySymbol& body)
    -> std::pair<DefinitionRepertoireDesc, RepertoireDebugView>;

// Text dump for inspection and testing.
// Format per artifact:
//   <kind> <identity> coord=[<step>, ...]
// where <step> is:
//   branch(ci=N,alt=M)     for kBranch
//   entry(ci=N,idx=M)      for kArrayEntry
auto DumpDefinitionRepertoireDesc(
    const DefinitionRepertoireDesc& desc, const RepertoireDebugView& debug_view)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
