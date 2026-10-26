#pragma once

#include <cstddef>
#include <map>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::lowering::ast_to_hir {

// The implementations the specializations of one unit have come out as, each
// held once under the name of the first that came out as it. Two are one
// where everything they state is equal, and only two of one unit can be; the
// text of each sorts them so that one is held against only those that could
// equal it, and is never taken for the comparison. The specializations of one
// unit are interned one after another, so a specialization of another unit
// lets go of everything held, and what is held is one unit's at a time.
class Implementations {
 public:
  // The name of the implementation `implementation` is: the one held equal to
  // it, where there is one, and otherwise `specialization`, under which it is
  // then held. It is asked before it is named, so it is held unnamed.
  auto Intern(
      std::string_view specialization,
      const hir::CompilationUnit& implementation) -> std::string;

 private:
  // The unit whose implementations are held.
  std::string unit_;
  std::vector<hir::CompilationUnit> held_;
  std::vector<std::string> names_;
  std::map<stable_hash::Fingerprint, std::vector<std::size_t>> by_text_;
};

}  // namespace lyra::lowering::ast_to_hir
