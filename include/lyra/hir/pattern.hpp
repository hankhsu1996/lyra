#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// The five pattern shapes SystemVerilog's pattern-matching surface introduces
// (LRM 12.6): a wildcard, a constant, a variable binding, a tagged-union
// discriminator, and a structure destructuring. HIR keeps the grammar's own
// hierarchy, because that is the level the user wrote; the desugar into a
// boolean test over tag and field primitives happens at HIR-to-MIR.

// LRM 12.6 wildcard pattern `.*`: matches any value, binds nothing.
struct WildcardPattern {};

// LRM 12.6 constant pattern: matches if the case value equals `value`.
struct ConstantPattern {
  ExprId value;
};

// LRM 12.6 variable pattern `.identifier`: always matches, and declares the
// identifier it names. LRM 12.6 puts that declaration in the pattern's own
// scope, so this node is the declaration, and a reference to the identifier
// names this node's `PatternId` (`PatternVarRef`). Keeping the declaration here
// is what lets one lowering serve every position a pattern can appear in. The
// identifier's type is the node's `subject_type`: the pattern binds whatever it
// is matched against, whole.
struct VariablePattern {
  std::string name;
};

// LRM 12.6 tagged pattern `tagged Member [pattern]`: matches iff the tagged
// union's active tag equals `member_index`; `value_pattern` (present for a
// non-void member) is then recursively matched against the payload. Member
// names are dropped -- position is the tag, consistent with the untagged /
// packed handling.
struct TaggedPattern {
  std::size_t member_index;
  std::optional<PatternId> value_pattern;
};

// LRM 12.6 structure pattern `'{...}`: always matches (types are known
// statically) and recursively matches each named field against its sub-
// pattern. Fields are indexed by position; omitted fields (LRM 12.6) are
// simply absent from the list.
struct StructurePattern {
  std::vector<std::pair<std::size_t, PatternId>> field_patterns;
};

using PatternData = std::variant<
    WildcardPattern, ConstantPattern, VariablePattern, TaggedPattern,
    StructurePattern>;

struct Pattern {
  PatternData data;
  // The type of the value this pattern is matched against. Resolving the
  // pattern already required it -- naming a member is what fixes its position
  // (LRM 12.6) -- so it is recorded where it was resolved. A consumer
  // descending the tree reads each level's type here instead of walking the
  // subject's type in step with the pattern and keeping the two aligned.
  TypeId subject_type;
  diag::SourceSpan span;
};

// One clause of an `if` / `?:` predicate. LRM 12.6.2 / 12.6.3 define the
// predicate as `&&&`-separated clauses evaluated as a sequential conjunction
// left to right, each either a Boolean expression or `expr matches pattern`,
// with a pattern's identifiers in scope for the remaining clauses and the
// true arm. The plain LRM 12.4 predicate is the degenerate case of that same
// grammar -- one clause, no pattern -- so it is represented here rather than
// as a separate shape.
struct ConditionClause {
  ExprId expr = {};
  std::optional<PatternId> pattern;
};

}  // namespace lyra::hir
