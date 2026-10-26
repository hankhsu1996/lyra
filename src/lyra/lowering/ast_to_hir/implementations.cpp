#include "lyra/lowering/ast_to_hir/implementations.hpp"

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::lowering::ast_to_hir {

auto Implementations::Intern(
    std::string_view specialization, const hir::CompilationUnit& implementation)
    -> std::string {
  if (implementation.name != unit_) {
    unit_ = implementation.name;
    held_.clear();
    names_.clear();
    by_text_.clear();
  }
  std::vector<std::size_t>& alike = by_text_[stable_hash::FingerprintOfText(
      hir::DumpImplementation(implementation))];
  for (const std::size_t held : alike) {
    if (held_[held] == implementation) return names_[held];
  }
  alike.push_back(held_.size());
  held_.push_back(implementation);
  names_.emplace_back(specialization);
  return names_.back();
}

}  // namespace lyra::lowering::ast_to_hir
