#pragma once

#include <cstddef>
#include <map>
#include <string>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::lowering::ast_to_hir {

// What interning one lowering's HIR answered: the name of the body it is, and
// whether this is the first lowering of its unit to come out as that body,
// which is the one that goes on to be compiled.
struct InternedBody {
  std::string name;
  bool compiles_its_body = false;
};

// The bodies the lowerings of one unit have come out as, each held
// once under its name. Two are one where everything they state is equal, and
// only two of one unit can be; the text of each sorts them so that one is held
// against only those that could equal it, and is never taken for the
// comparison. The lowerings of one unit are interned one after another, so a
// lowering of another unit lets go of everything held, and what is held is one
// unit's at a time.
//
// A body is named by its unit and which distinct body of the unit it is, in
// the order the unit's lowerings come out, the first being named as the unit
// is and each later one `unit#N`, N its ordinal among the unit's bodies and
// never an argument, joined by a character no simple identifier holds (LRM
// 5.6) so that no design element's name reads as a body of another's. An edit
// to a body keeps its name, so what names the body -- the design root's tables,
// an emitted file -- is not written again for it; a name moves only where a new
// distinct body comes out earlier, which is a change in what the unit's bodies
// are. A digest of the content would move with every edit.
class Bodies {
 public:
  // The body `lowered` comes out as: the one held equal to it, where there is
  // one, and otherwise a new one, under which it is then held.
  auto Intern(const hir::CompilationUnit& lowered) -> InternedBody;

 private:
  // The unit whose bodies are held.
  std::string unit_;
  std::vector<hir::CompilationUnit> held_;
  std::vector<std::string> names_;
  std::map<stable_hash::Fingerprint, std::vector<std::size_t>> by_text_;
};

}  // namespace lyra::lowering::ast_to_hir
