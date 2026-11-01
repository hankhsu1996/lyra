#include "lyra/lowering/ast_to_hir/bodies.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::lowering::ast_to_hir {

auto Bodies::Intern(const hir::CompilationUnit& lowered) -> InternedBody {
  if (lowered.name != unit_) {
    unit_ = lowered.name;
    held_.clear();
    names_.clear();
    by_text_.clear();
  }
  const stable_hash::Fingerprint text =
      stable_hash::FingerprintOfText(hir::DumpImplementation(lowered));
  std::vector<std::size_t>& alike = by_text_[text];
  for (const std::size_t held : alike) {
    if (held_[held] == lowered) {
      return InternedBody{.name = names_[held], .compiles_its_body = false};
    }
  }
  std::string name = held_.empty()
                         ? lowered.name
                         : std::format("{}#{}", lowered.name, held_.size());
  alike.push_back(held_.size());
  held_.push_back(lowered);
  names_.push_back(name);
  return InternedBody{.name = std::move(name), .compiles_its_body = true};
}

}  // namespace lyra::lowering::ast_to_hir
