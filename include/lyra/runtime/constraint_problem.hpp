#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/constraint_cycle.hpp"
#include "lyra/runtime/natural.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// One question a `randomize()` asks (LRM 18.6.1, 18.12): which values the
// variables it may choose take so that every constraint in force holds. Code
// the compiler generated describes the question by calls -- each variable the
// constraints read, each operation over them, each constraint -- and then asks
// for an answer and lands it, as a program using a constraint library would.
//
// Every value of the description is a term: a variable's bits, a constant's,
// or an operation's over terms, numbered in the order made. A variable is
// named by a reference to its storage, so two names of one storage are one
// variable. Whether it is chosen or held at what it holds is the call's to
// say: by the variables the call lists (LRM 18.11, 18.12), and otherwise by
// how the variable is declared and whether its mode is on (LRM 18.8). A
// variable held is a constant. Constraints are two-state (LRM 18.3), so an
// unknown bit a variable holds reads as zero.
//
// An answer is drawn from a generator split from the one the problem is
// handed -- the object's own for a class's `randomize()` (LRM 18.14.3), the
// running thread's for `std::randomize()` (LRM 18.14.2) -- uniformly among the
// assignments making
// every hard constraint true (LRM 18.5.9), after keeping each soft constraint
// that can still hold, the latest stated first (LRM 18.5.13.1), and choosing
// the variables an ordering names earlier first, each among the values the
// later ones can complete (LRM 18.5.9). Nothing is written until the answer is
// landed, and where there is none nothing is (LRM 18.6.3).
class ConstraintProblem final : public GcObject {
 public:
  // A problem of a call made in `runtime` drawing from `generator`, for a call
  // of `randomize()` on `object`, or of `std::randomize()` where it holds none.
  // Where `listed` is set, the variables the call lists are the ones chosen,
  // and nothing else is.
  ConstraintProblem(
      RuntimeEffects& runtime, DrawRng& generator, value::ObjectRef object,
      bool listed);
  ConstraintProblem(const ConstraintProblem&) = delete;
  auto operator=(const ConstraintProblem&) -> ConstraintProblem& = delete;
  ConstraintProblem(ConstraintProblem&&) = delete;
  auto operator=(ConstraintProblem&&) -> ConstraintProblem& = delete;
  ~ConstraintProblem() override;

  // The variable `lent` names is of an enumerated type, or is an array of
  // one, declaring `members`: what it takes where the call chooses it, and
  // what a `randc` one runs through (LRM 18.4.2, 18.5). Stated before it is
  // declared or listed, and answered by the storage the reference names.
  void Members(const ErasedReference& lent, const value::Enumeration& members);
  [[nodiscard]] auto MembersOf(const ErasedReference& lent) const
      -> const value::Enumeration*;

  // A variable the call lists, chosen whatever its declaration says (LRM
  // 18.11, 18.12), as an integral part of one, of the enumeration `members`
  // declares where it is one: an element of the array `within` answers where
  // it is one of an array whose size the program chooses. And such an array,
  // listed, answered as which array it is.
  void List(
      const ErasedReference& lent, const void* value,
      const value::ValueType& type, const value::Enumeration* members,
      std::optional<std::int64_t> within);
  [[nodiscard]] auto ListArray(
      const ErasedReference& whole, ArrayResizer resize) -> std::int64_t;

  // The storage `lent` names, holding `value` now, of the integral type
  // `type`, declared random as `declared` says with its mode `enabled`;
  // `cycle` is the order a `randc` variable runs through, which its owner
  // keeps; `members`, where the variable's type is an enumeration, the
  // members it declares. Every random variable is declared before any
  // constraint names one, so a constraint never reads one as the value it
  // holds. Storage declared already stays as it was. `within` is the array the
  // storage is an element of, where it is an element of one declared as an
  // array is below.
  void Declare(
      const ErasedReference& lent, const void* value,
      const value::ValueType& type, RandomQualifier declared, bool enabled,
      ConstraintCycle* cycle, const value::Enumeration* members,
      std::optional<std::int64_t> within = std::nullopt);

  // A dynamic array or a queue declared random (LRM 18.4), answered as which
  // array it is; its elements are declared after it, within it. Whether its
  // size is chosen is whether a constraint names it and its mode is
  // `enabled`.
  [[nodiscard]] auto DeclareArray(
      const ErasedReference& whole, ArrayResizer resize, bool enabled)
      -> std::int64_t;
  // The term for the size of the array the storage `whole` names, holding
  // `count` elements now: a variable where the array was declared random and
  // its size is not yet settled, and the size it has otherwise.
  [[nodiscard]] auto ArraySize(const ErasedReference& whole, std::size_t count)
      -> std::int64_t;
  // The variable `term` is one a function a constraint calls is handed, so
  // it is chosen before the constraints the call's answer enters (LRM
  // 18.5.11).
  void Want(std::int64_t term);
  // Settles what has to be chosen first: the size of every random array a
  // constraint sized, by the constraints that reach a size and no element of
  // an array being sized (LRM 18.5.7.1), and every variable a function is
  // handed, by the constraints reading no other random variable (LRM
  // 18.5.11). Each array is resized and each variable written, held from then
  // on, and the description is then stated again to a problem holding
  // nothing. False where nothing had to be, which leaves the problem as it
  // is, or where nothing could be, after which the call fails.
  [[nodiscard]] auto SettleFirst() -> bool;
  [[nodiscard]] auto Failed() const -> bool {
    return failed_;
  }
  // Puts back what settling changed, for a call that fails (LRM 18.6.3).
  void Abandon();
  // The term for the storage `lent` names, holding `value` now: the variable
  // declared there, or, where none was, a variable not declared random (LRM
  // 18.3).
  [[nodiscard]] auto Named(
      const ErasedReference& lent, const void* value,
      const value::ValueType& type) -> std::int64_t;
  // The term for a value the description reads and does not choose.
  [[nodiscard]] auto Constant(const void* value, const value::ValueType& type)
      -> std::int64_t;

  // Operations over terms (LRM 11.4), each as wide as its operands, which the
  // description has brought to one width; a comparison and a logical
  // operation answer one bit.
  [[nodiscard]] auto Negate(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto BitwiseNot(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto ReduceAnd(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto ReduceOr(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto ReduceXor(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto LogicalNot(std::int64_t a) -> std::int64_t;
  [[nodiscard]] auto Add(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto Subtract(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto Multiply(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto Divide(std::int64_t a, std::int64_t b, bool is_signed)
      -> std::int64_t;
  [[nodiscard]] auto Remainder(std::int64_t a, std::int64_t b, bool is_signed)
      -> std::int64_t;
  [[nodiscard]] auto BitwiseAnd(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto BitwiseOr(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto BitwiseXor(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto ShiftLeft(std::int64_t a, std::int64_t amount)
      -> std::int64_t;
  [[nodiscard]] auto ShiftRight(
      std::int64_t a, std::int64_t amount, bool arithmetic) -> std::int64_t;
  [[nodiscard]] auto Equal(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto Less(std::int64_t a, std::int64_t b, bool is_signed)
      -> std::int64_t;
  [[nodiscard]] auto LogicalAnd(std::int64_t a, std::int64_t b) -> std::int64_t;
  [[nodiscard]] auto LogicalOr(std::int64_t a, std::int64_t b) -> std::int64_t;
  // `high`'s bits above `low`'s, as a concatenation writes them (LRM 11.4.12).
  [[nodiscard]] auto Concatenate(std::int64_t high, std::int64_t low)
      -> std::int64_t;
  [[nodiscard]] auto Select(
      std::int64_t condition, std::int64_t then_term, std::int64_t else_term)
      -> std::int64_t;
  // A term at another width, extended by its sign or by zeros where wider.
  [[nodiscard]] auto Resize(
      std::int64_t a, std::int64_t width, bool sign_extend) -> std::int64_t;
  // `width` bits of a term from bit `offset` up.
  [[nodiscard]] auto Slice(
      std::int64_t a, std::int64_t offset, std::int64_t width) -> std::int64_t;

  // A constraint every answer meets (LRM 18.5), and one an answer meets where
  // it can (LRM 18.5.13); each holds where its term is nonzero.
  void Require(std::int64_t condition);
  void Prefer(std::int64_t condition);
  // Drops every soft constraint stated so far on the variable `variable` is
  // (LRM 18.5.13.2).
  void DisableSoft(std::int64_t variable);
  // The variable `before` is chosen before `after` (LRM 18.5.9).
  void SolveBefore(std::int64_t before, std::int64_t after);

  // A distribution of the value of `subject` (LRM 18.5.3), holding where
  // `guard` does, answered as which distribution it is; its items follow.
  // Its ranges compare as `is_signed` says. The subject is one of the values
  // an item of nonzero weight names, or any value at all where a default is
  // given, and which item's values it takes is chosen by weight among those
  // the constraints leave some value of; within it every value the
  // constraints allow is equally likely. A `soft` one holds where the other
  // constraints allow it (LRM 18.5.13).
  [[nodiscard]] auto Distribution(
      std::int64_t subject, std::int64_t guard, bool is_signed, bool soft)
      -> std::int64_t;
  // An item of the distribution `distribution`: the values from `low` to
  // `high`, weighing `weight` each where `per_value` is set (`:=`) and
  // `weight` together otherwise (`:/`). The weight is a value the
  // description reads, not one it chooses.
  void DistributionItem(
      std::int64_t distribution, std::int64_t low, std::int64_t high,
      std::int64_t weight, bool per_value);
  // `default :/ weight`: every value no item names, weighing `weight`
  // together.
  void DistributionDefault(std::int64_t distribution, std::int64_t weight);

  // A set of values that differ pairwise (LRM 18.5.4), answered as which set
  // it is; its members follow, and it is required, where `guard` holds, once
  // they are all named.
  [[nodiscard]] auto UniqueSet() -> std::int64_t;
  void UniqueMember(std::int64_t set, std::int64_t term);
  void UniqueRequire(std::int64_t set, std::int64_t guard);

  // The object a class's `randomize()` was called on, and the random objects
  // its handles reach (LRM 18.4, 18.6.2), each once; none for
  // `std::randomize()`.
  [[nodiscard]] auto Object() const -> const value::ObjectRef& {
    return object_;
  }
  // Adds `nested` unless it is already one of the call's objects.
  void AddNested(const value::ObjectRef& nested);
  [[nodiscard]] auto Nested() const -> const std::vector<value::ObjectRef>& {
    return nested_;
  }

  // Whether an answer exists; where one does it is drawn and kept for landing.
  [[nodiscard]] auto Solve() -> bool;
  // Writes the drawn answer into every chosen variable, each as an assignment
  // to it is written (LRM 18.6.1).
  void Commit();

 private:
  struct Variable {
    ErasedReference lent;
    const value::ValueType* type;
    std::uint32_t term;
    std::vector<std::uint32_t> inputs;
    bool random;
    ConstraintCycle* cycle;
    // The values its enumeration names, in declared order, where its type is
    // one: all it takes where it is chosen. A member holding x or z, which no
    // constraint gives a variable (LRM 18.3), is none.
    std::optional<std::vector<std::optional<std::vector<bool>>>> members;
    // The array whose size this variable is, which is resized rather than
    // written; none for a variable that is storage.
    std::optional<std::size_t> size_of;
  };
  struct RandomArray {
    ErasedReference whole;
    ArrayResizer resize;
    bool enabled;
    std::vector<std::size_t> elements;
    std::optional<std::size_t> size;
  };
  struct Soft {
    CircuitLiteral condition;
    std::vector<std::uint32_t> support;
    bool disabled = false;
  };

  // The bits of a term the description reads rather than chooses, least
  // significant first, which a weight's and a range's bounds are.
  [[nodiscard]] auto HeldBits(std::int64_t term) const -> std::vector<bool>;
  // Which item of each distribution its subject takes, each required where
  // its guard holds, `condition` being every constraint so far; false where
  // a distribution required to hold has no item the constraints leave room
  // for.
  [[nodiscard]] auto ChooseDistributions(CircuitLiteral& condition) -> bool;

  [[nodiscard]] auto Chosen(
      RandomQualifier declared, bool enabled, const void* at) const -> bool;
  // A new variable, answering its term.
  auto AddVariable(
      const ErasedReference& lent, const void* value,
      const value::ValueType& type, bool random, ConstraintCycle* cycle,
      const value::Enumeration* members) -> std::int64_t;
  // The generator every draw of the call takes from.
  [[nodiscard]] auto CallGenerator() -> DrawRng&;
  // The values `variable` may take where it is chosen: the members its
  // enumeration names, or none where it may take any value.
  [[nodiscard]] static auto MemberValues(const Variable& variable)
      -> std::vector<std::vector<bool>>;
  // `variable` holding the value whose bits are `bits`, low bit first.
  [[nodiscard]] auto Pin(
      const Variable& variable, const std::vector<bool>& bits)
      -> CircuitLiteral;
  // Forgets everything stated, keeping which objects the call reaches and
  // what it lists.
  void Restart();
  // Chooses each `randc` variable's next value before anything else (LRM
  // 18.4.2), pinning it in `condition`; false where one has none left.
  [[nodiscard]] auto ChooseCycles(CircuitLiteral& condition) -> bool;
  [[nodiscard]] auto Bits(std::int64_t term) const -> const CircuitBits&;
  [[nodiscard]] auto Make(CircuitBits bits) -> std::int64_t;
  [[nodiscard]] auto Truth(std::int64_t term) -> CircuitLiteral;
  [[nodiscard]] auto VariableOf(std::int64_t term) const
      -> std::optional<std::size_t>;
  [[nodiscard]] auto SameWidth(std::int64_t a, std::int64_t b) const
      -> const CircuitBits&;

  // The run the call is made in, whose command line bounds how far a call
  // counts solutions and to which a draw past that is reported.
  RuntimeEffects* runtime_;
  DrawRng* generator_;
  // What the call draws from: a generator split from `generator_` at the
  // first draw, after `pre_randomize` has run and may have seeded it (LRM
  // 18.13.3), so the object's own generator advances by the same amount
  // whatever the call draws, once it chooses anything.
  std::optional<DrawRng> call_generator_;
  value::ObjectRef object_;
  std::vector<value::ObjectRef> nested_;
  // The storage the call lists, where it lists any.
  std::optional<std::unordered_set<const void*>> listed_;
  ConstraintCircuit circuit_;
  std::vector<CircuitBits> terms_;
  std::vector<Variable> variables_;
  std::unordered_map<const void*, std::size_t> variable_at_;
  std::unordered_map<const void*, value::Enumeration> members_;
  std::vector<CircuitLiteral> hard_;
  std::vector<Soft> soft_;
  std::vector<std::pair<std::size_t, std::size_t>> order_;
  struct WeightedValues {
    CircuitLiteral member;
    Natural weight;
  };
  struct WeightedChoice {
    CircuitBits subject;
    CircuitLiteral guard;
    bool is_signed;
    bool soft;
    std::vector<WeightedValues> items;
    std::optional<Natural> default_weight;
  };
  std::vector<WeightedChoice> distributions_;
  std::vector<std::vector<std::int64_t>> unique_;
  std::vector<RandomArray> arrays_;
  std::unordered_map<const void*, std::size_t> array_at_;
  // The variables wanted first, by their index, until settled.
  std::unordered_set<std::size_t> wanted_;
  // The storage settled first, held from then on.
  std::unordered_set<const void*> settled_;
  // How to put back each change settling made, in the order made.
  std::vector<std::function<void()>> undo_;
  bool settled_first_ = false;
  bool failed_ = false;
  // Whether some draw of the call was made past the budget.
  bool drew_past_budget_ = false;
  // The value drawn for each input of the circuit, once solved.
  std::vector<bool> answer_;
  // The value each `randc` variable took, which landing takes from its cycle.
  std::vector<std::pair<ConstraintCycle*, std::uint64_t>> cycled_;
};

}  // namespace lyra::runtime
