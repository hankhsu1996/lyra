#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/id_range.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// What a bin construct counts toward (LRM 19.5, 19.5.5, 19.5.6).
enum class BinKind : std::uint8_t {
  kBins,
  kIgnoreBins,
  kIllegalBins,
};

// One element of a bin's range list (LRM 19.5.1): a value, a range whose ends
// the source may leave open with `$`, or a range written as a center and a
// tolerance (LRM 11.4.13).
struct BinValue {
  ExprId value;

  auto operator==(const BinValue&) const -> bool = default;
};

struct BinRange {
  // Absent where the source wrote `$`.
  std::optional<ExprId> low;
  std::optional<ExprId> high;

  auto operator==(const BinRange&) const -> bool = default;
};

struct BinTolerance {
  ExprId center;
  ExprId tolerance;
  bool in_percent = false;

  auto operator==(const BinTolerance&) const -> bool = default;
};

using BinElement = std::variant<BinValue, BinRange, BinTolerance>;

// How many bins one construct makes (LRM 19.5.1): one, one per value (`b[]`),
// or the number its brackets give (`b[N]`).
struct OneBin {
  auto operator==(const OneBin&) const -> bool = default;
};

struct BinPerValue {
  auto operator==(const BinPerValue&) const -> bool = default;
};

struct FixedBins {
  ExprId count;

  auto operator==(const FixedBins&) const -> bool = default;
};

using BinArray = std::variant<OneBin, BinPerValue, FixedBins>;

// A `with` filter (LRM 19.5.1.1): each value the bins would hold is kept where
// `keeps` holds of it, the value being `item`.
struct BinFilter {
  ProceduralVarId item;
  ExprId keeps;

  auto operator==(const BinFilter&) const -> bool = default;
};

// Bins of values (LRM 19.5.1): from a range list, or from an array or queue the
// set expression evaluates to (LRM 19.5.1.2), each value compared under the
// mask a wildcard bin gives (LRM 19.5.4).
struct ValueBins {
  BinArray array;
  bool wildcard = false;
  std::vector<BinElement> elements;
  std::optional<ExprId> set;
  std::optional<BinFilter> filter;

  auto operator==(const ValueBins&) const -> bool = default;
};

// How a transition step repeats (LRM 19.5.2).
enum class StepRepetition : std::uint8_t {
  kOnce,
  kConsecutive,
  kGoTo,
  kNonconsecutive,
};

struct TransitionStep {
  std::vector<BinElement> values;
  StepRepetition repetition = StepRepetition::kOnce;
  // Absent where the source wrote no count, and `most` absent where it wrote
  // one count, which is then the most as well (LRM 19.5.2).
  std::optional<ExprId> least;
  std::optional<ExprId> most;

  auto operator==(const TransitionStep&) const -> bool = default;
};

// Bins of value sequences (LRM 19.5.2): one bin for every sequence the list
// names, or with `per_sequence` (`b[]`) one for each sequence of values.
struct TransitionBins {
  bool per_sequence = false;
  bool wildcard = false;
  std::vector<std::vector<TransitionStep>> sequences;

  auto operator==(const TransitionBins&) const -> bool = default;
};

// The bin of every value no other bin of the coverpoint holds (LRM 19.5), one
// per value where the source wrote `default[]`.
struct DefaultBins {
  bool per_value = false;

  auto operator==(const DefaultBins&) const -> bool = default;
};

// The bin of every sequence no transition bin completes (LRM 19.5.2).
struct DefaultSequenceBins {
  auto operator==(const DefaultSequenceBins&) const -> bool = default;
};

using BinForm =
    std::variant<ValueBins, TransitionBins, DefaultBins, DefaultSequenceBins>;

// One bin construct of a coverpoint, in the order the source declares them.
struct BinConstruct {
  std::string name;
  BinKind kind = BinKind::kBins;
  std::optional<ExprId> iff;
  BinForm form;

  auto operator==(const BinConstruct&) const -> bool = default;
};

// `option.<field> = value` or `type_option.<field> = value` written in a
// covergroup, a coverpoint or a cross (LRM 19.7).
struct OptionSetting {
  bool type_option = false;
  std::string field;
  ExprId value;

  auto operator==(const OptionSetting&) const -> bool = default;
};

// A coverpoint (LRM 19.5): the expression sampled, its guard, its options and
// its bins. A coverpoint the source gave no label is named by the compiler, by
// a name no identifier can spell. What counts its values is published with
// the covergroup, at the coverpoint's position.
struct Coverpoint {
  std::string name;
  ExprId expression;
  std::optional<ExprId> iff;
  std::vector<OptionSetting> options;
  std::vector<BinConstruct> bins;

  auto operator==(const Coverpoint&) const -> bool = default;
};

// A cross bin's selection of the cross's tuples (LRM 19.6.1), as a tree whose
// nodes are held by the cross and named by their position there.
struct CrossSelectId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CrossSelectId&) const
      -> std::strong_ordering = default;
};

// Every tuple of the cross.
struct SelectAll {
  auto operator==(const SelectAll&) const -> bool = default;
};

// `binsof(cp)` or `binsof(cp.bin)`, `intersect` a range list where written: the
// tuples whose component of the coverpoint `point` names is a bin it names.
struct SelectBinsOf {
  std::string point;
  std::optional<std::string> bin;
  std::vector<BinElement> intersect;

  auto operator==(const SelectBinsOf&) const -> bool = default;
};

struct SelectNot {
  CrossSelectId operand;

  auto operator==(const SelectNot&) const -> bool = default;
};

struct SelectAnd {
  CrossSelectId left;
  CrossSelectId right;

  auto operator==(const SelectAnd&) const -> bool = default;
};

struct SelectOr {
  CrossSelectId left;
  CrossSelectId right;

  auto operator==(const SelectOr&) const -> bool = default;
};

// How many of a tuple's value combinations a filter or a set has to hold for
// the tuple to be selected (LRM 19.6.1.2): one where the source wrote no
// `matches`, every one for `matches $`, or the number written.
struct OneMatch {
  auto operator==(const OneMatch&) const -> bool = default;
};

struct EveryMatch {
  auto operator==(const EveryMatch&) const -> bool = default;
};

using MatchesRequired = std::variant<OneMatch, EveryMatch, ExprId>;

// `select with (filter) matches n` (LRM 19.6.1.2): the tuples of `candidates`
// for which enough of the value combinations of their bins satisfy `filter`,
// each coverpoint's value being its variable of `values` -- in the order of
// the cross's targets, absent for one the filter does not name.
struct SelectWith {
  CrossSelectId candidates;
  std::vector<std::optional<ProceduralVarId>> values;
  ExprId filter;
  MatchesRequired matches;

  auto operator==(const SelectWith&) const -> bool = default;
};

// A set expression of the cross's tuple type (LRM 19.6.1.4).
struct SelectSet {
  ExprId tuples;
  MatchesRequired matches;

  auto operator==(const SelectSet&) const -> bool = default;
};

using CrossSelect = std::variant<
    SelectAll, SelectBinsOf, SelectNot, SelectAnd, SelectOr, SelectWith,
    SelectSet>;

struct CrossBin {
  std::string name;
  BinKind kind = BinKind::kBins;
  CrossSelectId select;
  std::optional<ExprId> iff;

  auto operator==(const CrossBin&) const -> bool = default;
};

// A cross (LRM 19.6) of coverpoints of the same covergroup or of the one it
// extends, named as the source names them.
struct Cross {
  std::string name;
  std::vector<std::string> targets;
  std::optional<ExprId> iff;
  std::vector<OptionSetting> options;
  base::Arena<CrossSelect, CrossSelectId> selects;
  std::vector<CrossBin> bins;

  auto operator==(const Cross&) const -> bool = default;
};

// A block whose beginning or end samples a covergroup (LRM 19.3): a named
// block, or the body of a task, function or method, of the instance or class
// declaring the covergroup, named by its scope there.
struct BlockEvent {
  ProceduralScopeId block;
  bool at_begin = false;

  auto operator==(const BlockEvent&) const -> bool = default;
};

// The covergroup is sampled only by calling `sample`.
struct SampledByCall {
  auto operator==(const SampledByCall&) const -> bool = default;
};

// The covergroup is sampled at each occurrence of an event (LRM 19.3), whose
// expressions are the covergroup's.
struct SampledOnEvent {
  EventControl event;

  auto operator==(const SampledOnEvent&) const -> bool = default;
};

// The covergroup is sampled where blocks begin or end (LRM 19.3).
struct SampledAtBlocks {
  std::vector<BlockEvent> events;

  auto operator==(const SampledAtBlocks&) const -> bool = default;
};

using CoverageSampling =
    std::variant<SampledByCall, SampledOnEvent, SampledAtBlocks>;

// A covergroup (LRM 19.3) as the source declares it, in a module, an interface,
// a program, a package, the compilation-unit scope or a class: its formals,
// which its expressions read; the formals of an overriding `sample` (LRM
// 19.8.1); how it is sampled; its options; its coverpoints and crosses. Every
// expression it holds lives in `body`, and every variable those expressions
// read that the source did not declare outside the covergroup -- a formal, a
// formal of `sample`, the object embedding it, the `item` of a filter -- is a
// variable of `body`.
//
// A covergroup a class embeds (LRM 19.4) reads the embedding object's members
// through `embedding`, the object it is made by. One extending another (LRM
// 19.4.1) names `base`, takes its formals and coverpoints, and lists only what
// it adds or declares again.
struct CovergroupDecl {
  std::vector<SubroutineParam> formals;
  std::vector<SubroutineParam> sample_formals;
  std::optional<ProceduralVarId> embedding;
  CoverageSampling sampling;
  std::vector<OptionSetting> options;
  std::vector<Coverpoint> coverpoints;
  std::vector<Cross> crosses;
  std::optional<CovergroupRef> base;
  ProceduralBody body;
  base::Registry<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;
  // The name of the scope declaring the covergroup, as far as its text fixes
  // it, which the covergroup's own name follows (LRM 21.2.1.5).
  FixedName declared_in;
  diag::SourceSpan span;

  auto operator==(const CovergroupDecl&) const -> bool = default;
};

// The classes of the coverage library every covergroup of a unit is written
// over in the step to MIR (LRM 19), as that unit names them, as a language
// names the library items its constructs stand on.
struct CoverageLibraryRefs {
  ClassRef registry;
  ClassRef group_type;
  ClassRef group_instance;
  ClassRef cross;
  ClassRef selection;

  auto operator==(const CoverageLibraryRefs&) const -> bool = default;
};

// The covergroups one compilation unit declares, each named by its path in the
// unit from the moment the source names it, as a class is (LRM 26.3).
class CovergroupRegistry {
 public:
  auto Declare(support::DefPath path) -> CovergroupId {
    paths_.push_back(std::move(path));
    published_.emplace_back();
    return decls_.Declare();
  }

  // What the unit publishes of the covergroup, computed once where it is
  // declared: every layout of its class, in this unit and in any unit naming
  // it, is read off this one form.
  void Publish(CovergroupId id, PublishedCovergroup published) {
    published_.at(id.value) = std::move(published);
  }

  [[nodiscard]] auto PublishedOf(CovergroupId id) const
      -> const std::optional<PublishedCovergroup>& {
    return published_.at(id.value);
  }

  void Define(CovergroupId id, CovergroupDecl decl) {
    decls_.Define(id, std::move(decl));
  }

  [[nodiscard]] auto PathOf(CovergroupId id) const -> const support::DefPath& {
    return paths_.at(id.value);
  }

  [[nodiscard]] auto Get(CovergroupId id) const -> const CovergroupDecl& {
    return decls_.Get(id);
  }

  [[nodiscard]] auto IsDefined(CovergroupId id) const -> bool {
    return decls_.IsDefined(id);
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<CovergroupId> {
    return decls_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return decls_.size();
  }

  auto operator==(const CovergroupRegistry&) const -> bool = default;

 private:
  base::Registry<CovergroupDecl, CovergroupId> decls_;
  std::vector<support::DefPath> paths_;
  std::vector<std::optional<PublishedCovergroup>> published_;
};

}  // namespace lyra::hir
