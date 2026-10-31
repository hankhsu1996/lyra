#include "lyra/runtime/constraint_problem.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <numeric>
#include <optional>
#include <ranges>
#include <span>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/constraint_bdd.hpp"
#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/constraint_sampler.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/natural.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

namespace {

auto WidthOf(const value::ValueType& type) -> std::uint64_t {
  const value::IntegralValueType* integral = type.AsIntegral();
  if (integral == nullptr) {
    throw InternalError(
        "ConstraintProblem: a constraint reads a value that is not integral -- "
        "please report this as a bug");
  }
  return integral->Shape().width;
}

auto WordsFor(std::uint64_t width) -> std::size_t {
  return static_cast<std::size_t>((width + 63) / 64);
}

// Each member of `members` as its bits, low bit first, in declared order; none
// for a member holding x or z, which no constraint gives a variable (LRM
// 18.3).
auto MemberBits(const value::Enumeration& members)
    -> std::vector<std::optional<std::vector<bool>>> {
  std::vector<std::optional<std::vector<bool>>> out;
  for (std::size_t position = 0; position < members.members; ++position) {
    const value::ConstIntegralView member = members.MemberAt(position);
    if (std::ranges::any_of(
            member.unknown, [](std::uint64_t word) { return word != 0; })) {
      out.emplace_back(std::nullopt);
      continue;
    }
    std::vector<bool> bits(members.width);
    for (std::uint64_t i = 0; i < members.width; ++i) {
      bits[i] = ((member.value[i / 64] >> (i % 64)) & 1U) != 0;
    }
    out.emplace_back(std::move(bits));
  }
  return out;
}

// The operations of the integral type `type`, which a random variable is.
auto IntegralOperationsOf(const value::ValueType& type)
    -> value::ValueOperations {
  return {
      .functions = &value::lyra_rt_integral_operation_functions,
      .type = &type,
      .element = nullptr};
}

// The bits of a value of `type` at `value`, an unknown bit read as zero
// (LRM 18.3: constraints are two-state).
auto BitsOf(const void* value, const value::ValueType& type)
    -> std::vector<bool> {
  const std::uint64_t width = WidthOf(type);
  std::vector<std::uint64_t> words(WordsFor(width), 0);
  std::vector<std::uint64_t> unknown(WordsFor(width), 0);
  const value::ValueOperations operations = IntegralOperationsOf(type);
  operations.functions->write_to_stream(
      operations, value,
      value::IntegralView{.value = words, .unknown = unknown, .width = width},
      0);
  std::vector<bool> bits(width);
  for (std::uint64_t i = 0; i < width; ++i) {
    const std::uint64_t word = i / 64;
    const std::uint64_t bit = i % 64;
    const bool known = ((unknown[word] >> bit) & 1U) == 0;
    bits[i] = known && ((words[word] >> bit) & 1U) != 0;
  }
  return bits;
}

// Forward: writes `bits`, the least significant first, into the storage
// `lent` names, of the integral type `type`, as an assignment is written.
void WriteBits(
    const ErasedReference& lent, const value::ValueType& type,
    const std::vector<bool>& bits);

// Storage for one value of a type the problem knows only at run time, ended
// when it goes.
class Scratch {
 public:
  explicit Scratch(const value::ValueType& type)
      : type_(&type), bytes_(type.Size() + type.Align()) {
  }
  Scratch(const Scratch&) = delete;
  auto operator=(const Scratch&) -> Scratch& = delete;
  Scratch(Scratch&&) = delete;
  auto operator=(Scratch&&) -> Scratch& = delete;
  ~Scratch() {
    if (built_) {
      type_->Destroy(Address());
    }
  }

  [[nodiscard]] auto Address() -> void* {
    void* at = bytes_.data();
    std::size_t space = bytes_.size();
    return std::align(type_->Align(), type_->Size(), at, space);
  }
  void MarkBuilt() {
    built_ = true;
  }

 private:
  const value::ValueType* type_;
  std::vector<std::byte> bytes_;
  bool built_ = false;
};

void WriteBits(
    const ErasedReference& lent, const value::ValueType& type,
    const std::vector<bool>& bits) {
  const std::uint64_t width = WidthOf(type);
  std::vector<std::uint64_t> words(WordsFor(width), 0);
  for (std::uint64_t i = 0; i < width; ++i) {
    if (bits[i]) {
      words[i / 64] |= std::uint64_t{1} << (i % 64);
    }
  }
  Scratch value(type);
  const value::ValueOperations operations = IntegralOperationsOf(type);
  operations.functions->read_from_stream(
      operations,
      value::ConstIntegralView{.value = words, .unknown = {}, .width = width},
      0, width, value.Address());
  value.MarkBuilt();
  WriteLaidOutThrough(lent, type, value.Address());
}

}  // namespace

const ObjectDefinition ConstraintProblem::kDefinition =
    LibraryObjectDefinition<ConstraintProblem>();

ConstraintProblem::ConstraintProblem(
    RuntimeEffects& runtime, DrawRng& generator, value::ObjectRef object,
    bool listed)
    : GcObject(kDefinition),
      runtime_(&runtime),
      generator_(&generator),
      object_(std::move(object)) {
  if (listed) {
    listed_.emplace();
  }
}

ConstraintProblem::~ConstraintProblem() = default;

auto ConstraintProblem::Bits(std::int64_t term) const -> const CircuitBits& {
  if (term < 0 || static_cast<std::size_t>(term) >= terms_.size()) {
    throw InternalError(
        "ConstraintProblem: a term that was never made -- please report this "
        "as a bug");
  }
  return terms_[static_cast<std::size_t>(term)];
}

auto ConstraintProblem::Make(CircuitBits bits) -> std::int64_t {
  terms_.push_back(std::move(bits));
  return static_cast<std::int64_t>(terms_.size() - 1);
}

auto ConstraintProblem::SameWidth(std::int64_t a, std::int64_t b) const
    -> const CircuitBits& {
  if (Bits(a).size() != Bits(b).size()) {
    throw InternalError(
        "ConstraintProblem: an operation over terms of two widths -- please "
        "report this as a bug");
  }
  return Bits(b);
}

auto ConstraintProblem::Truth(std::int64_t term) -> CircuitLiteral {
  return circuit_.ReduceOr(Bits(term));
}

auto ConstraintProblem::Chosen(
    RandomQualifier declared, bool enabled, const void* at) const -> bool {
  if (settled_.contains(at)) {
    return false;
  }
  if (listed_.has_value()) {
    return listed_->contains(at);
  }
  switch (declared) {
    case RandomQualifier::kNone:
      return false;
    case RandomQualifier::kRand:
    case RandomQualifier::kRandc:
      return enabled;
  }
  throw InternalError("ConstraintProblem: unknown declaration of a variable");
}

auto ConstraintProblem::AddVariable(
    const ErasedReference& lent, const void* value,
    const value::ValueType& type, bool random, ConstraintCycle* cycle,
    const value::Enumeration* members) -> std::int64_t {
  CircuitBits bits;
  std::vector<std::uint32_t> inputs;
  if (random) {
    for (std::uint64_t i = 0; i < WidthOf(type); ++i) {
      const CircuitLiteral input = circuit_.Input();
      bits.push_back(input);
      inputs.push_back(circuit_.InputIndex(input.Node()));
    }
  } else {
    bits = ConstraintCircuit::Constant(BitsOf(value, type));
  }
  // A chosen variable of an enumerated type takes one of the values the
  // enumeration names, whatever else its type could hold (LRM 18.5).
  std::optional<std::vector<std::optional<std::vector<bool>>>> named;
  if (random && members != nullptr) {
    named = MemberBits(*members);
    CircuitLiteral any = kFalseLiteral;
    for (const std::optional<std::vector<bool>>& member : *named) {
      if (member.has_value()) {
        any = circuit_.Or(
            any, circuit_.Equal(bits, ConstraintCircuit::Constant(*member)));
      }
    }
    hard_.push_back(any);
  }
  const auto term = static_cast<std::uint32_t>(Make(std::move(bits)));
  variable_at_.emplace(lent.storage, variables_.size());
  variables_.push_back(
      Variable{
          .lent = lent,
          .type = &type,
          .term = term,
          .inputs = std::move(inputs),
          .random = random,
          .cycle = random ? cycle : nullptr,
          .members = std::move(named),
          .size_of = std::nullopt});
  return term;
}

void ConstraintProblem::List(
    const ErasedReference& lent, const void* value,
    const value::ValueType& type, const value::Enumeration* members,
    std::optional<std::int64_t> within) {
  if (!listed_.has_value()) {
    throw InternalError(
        "ConstraintProblem: a variable is listed for a call that lists none -- "
        "please report this as a bug");
  }
  listed_->insert(lent.storage);
  if (variable_at_.contains(lent.storage)) {
    return;
  }
  const std::size_t index = variables_.size();
  AddVariable(lent, value, type, true, nullptr, members);
  if (within.has_value()) {
    arrays_.at(static_cast<std::size_t>(*within)).elements.push_back(index);
  }
}

auto ConstraintProblem::ListArray(
    const ErasedReference& whole, ArrayResizer resize) -> std::int64_t {
  listed_->insert(whole.storage);
  return DeclareArray(whole, std::move(resize), true);
}

void ConstraintProblem::Members(
    const ErasedReference& lent, const value::Enumeration& members) {
  members_.insert_or_assign(lent.storage, members);
}

auto ConstraintProblem::MembersOf(const ErasedReference& lent) const
    -> const value::Enumeration* {
  const auto it = members_.find(lent.storage);
  return it == members_.end() ? nullptr : &it->second;
}

void ConstraintProblem::Declare(
    const ErasedReference& lent, const void* value,
    const value::ValueType& type, RandomQualifier declared, bool enabled,
    ConstraintCycle* cycle, const value::Enumeration* members,
    std::optional<std::int64_t> within) {
  if (const auto it = variable_at_.find(lent.storage);
      it != variable_at_.end()) {
    Variable& variable = variables_[it->second];
    // A listed variable keeps its cyclic mode (LRM 18.11), which only its
    // declaration knows.
    if (variable.random && declared == RandomQualifier::kRandc &&
        variable.cycle == nullptr) {
      variable.cycle = cycle;
    }
    return;
  }
  const std::size_t index = variables_.size();
  AddVariable(
      lent, value, type, Chosen(declared, enabled, lent.storage),
      declared == RandomQualifier::kRandc ? cycle : nullptr, members);
  if (within.has_value()) {
    arrays_.at(static_cast<std::size_t>(*within)).elements.push_back(index);
  }
}

auto ConstraintProblem::DeclareArray(
    const ErasedReference& whole, ArrayResizer resize, bool enabled)
    -> std::int64_t {
  if (const auto it = array_at_.find(whole.storage); it != array_at_.end()) {
    return static_cast<std::int64_t>(it->second);
  }
  // A size the sizes were settled to is held from then on, and so is one a
  // listing call does not name (LRM 18.11).
  const bool chosen =
      !settled_first_ && Chosen(RandomQualifier::kRand, enabled, whole.storage);
  array_at_.emplace(whole.storage, arrays_.size());
  arrays_.push_back(
      RandomArray{
          .whole = whole,
          .resize = std::move(resize),
          .enabled = chosen,
          .elements = {},
          .size = std::nullopt});
  return static_cast<std::int64_t>(arrays_.size() - 1);
}

auto ConstraintProblem::ArraySize(
    const ErasedReference& whole, std::size_t count) -> std::int64_t {
  // A size is an `int` (LRM 7.5.2), never negative.
  constexpr std::size_t kSizeWidth = 32;
  const auto it = array_at_.find(whole.storage);
  if (it == array_at_.end() || !arrays_[it->second].enabled) {
    std::vector<bool> bits(kSizeWidth);
    for (std::size_t i = 0; i < kSizeWidth; ++i) {
      bits[i] = i < 64 && ((count >> i) & 1U) != 0;
    }
    return Make(ConstraintCircuit::Constant(bits));
  }
  RandomArray& array = arrays_[it->second];
  if (array.size.has_value()) {
    return variables_[*array.size].term;
  }
  CircuitBits bits;
  std::vector<std::uint32_t> inputs;
  for (std::size_t i = 0; i < kSizeWidth; ++i) {
    const CircuitLiteral input = circuit_.Input();
    bits.push_back(input);
    inputs.push_back(circuit_.InputIndex(input.Node()));
  }
  hard_.push_back(bits.back().Negated());
  const auto term = static_cast<std::uint32_t>(Make(std::move(bits)));
  array.size = variables_.size();
  variables_.push_back(
      Variable{
          .lent = whole,
          .type = nullptr,
          .term = term,
          .inputs = std::move(inputs),
          .random = true,
          .cycle = nullptr,
          .members = std::nullopt,
          .size_of = it->second});
  return term;
}

void ConstraintProblem::Restart() {
  circuit_ = ConstraintCircuit{};
  terms_.clear();
  variables_.clear();
  variable_at_.clear();
  members_.clear();
  hard_.clear();
  soft_.clear();
  order_.clear();
  distributions_.clear();
  unique_.clear();
  arrays_.clear();
  array_at_.clear();
  answer_.clear();
  cycled_.clear();
}

void ConstraintProblem::Want(std::int64_t term) {
  const std::optional<std::size_t> v = VariableOf(term);
  if (v.has_value() && variables_[*v].random &&
      !variables_[*v].size_of.has_value()) {
    wanted_.insert(*v);
  }
}

namespace {

// The items of `items` that `taken` marks, in order. Each is moved into a new
// list, since moving one onto itself would leave it empty.
template <class T>
void Keep(std::vector<T>& items, const std::vector<bool>& taken) {
  std::vector<T> kept;
  for (std::size_t i = 0; i < items.size(); ++i) {
    if (taken[i]) {
      kept.push_back(std::move(items[i]));
    }
  }
  items = std::move(kept);
}

}  // namespace

auto ConstraintProblem::SettleFirst() -> bool {
  if (settled_first_) {
    return false;
  }
  settled_first_ = true;
  const std::uint32_t inputs = circuit_.InputCount();
  // The inputs of what is chosen first, of the elements of an array being
  // sized, and of every other random variable.
  std::vector<bool> sized(inputs, false);
  std::vector<bool> element(inputs, false);
  std::vector<bool> first(inputs, false);
  std::vector<bool> random(inputs, false);
  bool any = !wanted_.empty();
  for (const RandomArray& array : arrays_) {
    if (!array.size.has_value()) {
      continue;
    }
    any = true;
    for (const std::uint32_t input : variables_[*array.size].inputs) {
      sized[input] = true;
    }
    for (const std::size_t e : array.elements) {
      for (const std::uint32_t input : variables_[e].inputs) {
        element[input] = true;
      }
    }
  }
  if (!any) {
    return false;
  }
  for (const std::size_t v : wanted_) {
    for (const std::uint32_t input : variables_[v].inputs) {
      first[input] = true;
    }
  }
  for (const Variable& variable : variables_) {
    for (const std::uint32_t input : variable.inputs) {
      random[input] = true;
    }
  }

  const auto support_of = [&](CircuitLiteral condition) {
    const std::array roots{condition};
    return circuit_.Support(roots);
  };
  std::vector<std::vector<std::uint32_t>> hard_support;
  hard_support.reserve(hard_.size());
  for (const CircuitLiteral condition : hard_) {
    hard_support.push_back(support_of(condition));
  }
  std::vector<std::vector<std::uint32_t>> choice_support;
  choice_support.reserve(distributions_.size());
  for (const WeightedChoice& choice : distributions_) {
    CircuitLiteral all = choice.guard;
    for (const WeightedValues& item : choice.items) {
      all = circuit_.And(all, item.member);
    }
    choice_support.push_back(support_of(all));
  }
  const auto any_of = [](std::span<const std::uint32_t> support,
                         const std::vector<bool>& set) {
    return std::ranges::any_of(
        support, [&](std::uint32_t input) { return set[input]; });
  };
  // A constraint is solved first where it reaches a size through what it
  // shares with others solved first, reading no element of an array being
  // sized, or where every random variable it reads is one a function is
  // handed.
  const auto first_only = [&](std::span<const std::uint32_t> support) {
    return any_of(support, first) &&
           std::ranges::all_of(support, [&](std::uint32_t input) {
             return !random[input] || first[input];
           });
  };
  std::vector<bool> hard_taken(hard_.size(), false);
  std::vector<bool> soft_taken(soft_.size(), false);
  std::vector<bool> choice_taken(distributions_.size(), false);
  const auto take = [&](std::span<const std::uint32_t> support,
                        std::vector<bool>& taken, std::size_t at) {
    if (taken[at]) {
      return false;
    }
    if (first_only(support)) {
      taken[at] = true;
      return false;
    }
    if (any_of(support, element) || !any_of(support, sized)) {
      return false;
    }
    taken[at] = true;
    for (const std::uint32_t input : support) {
      sized[input] = true;
    }
    return true;
  };
  for (bool grew = true; grew;) {
    grew = false;
    for (std::size_t c = 0; c < hard_.size(); ++c) {
      grew = take(hard_support[c], hard_taken, c) || grew;
    }
    for (std::size_t s = 0; s < soft_.size(); ++s) {
      grew = take(soft_[s].support, soft_taken, s) || grew;
    }
    for (std::size_t d = 0; d < distributions_.size(); ++d) {
      grew = take(choice_support[d], choice_taken, d) || grew;
    }
  }
  Keep(hard_, hard_taken);
  Keep(soft_, soft_taken);
  Keep(distributions_, choice_taken);

  if (!Solve()) {
    failed_ = true;
    return false;
  }
  const auto value_of = [&](const Variable& variable) {
    std::vector<bool> bits(variable.inputs.size());
    for (std::size_t i = 0; i < bits.size(); ++i) {
      bits[i] = answer_[variable.inputs[i]];
    }
    return bits;
  };
  std::vector<std::pair<std::size_t, std::size_t>> sizes;
  for (std::size_t a = 0; a < arrays_.size(); ++a) {
    if (!arrays_[a].size.has_value()) {
      continue;
    }
    std::size_t count = 0;
    const std::vector<bool> bits = value_of(variables_[*arrays_[a].size]);
    for (std::size_t i = 0; i < bits.size(); ++i) {
      if (bits[i]) {
        count |= std::size_t{1} << i;
      }
    }
    sizes.emplace_back(a, count);
  }
  std::vector<std::pair<std::size_t, std::vector<bool>>> values;
  for (const std::size_t v : wanted_) {
    values.emplace_back(v, value_of(variables_[v]));
  }
  for (const auto& [a, count] : sizes) {
    undo_.push_back(arrays_[a].resize(count));
  }
  for (const auto& [v, bits] : values) {
    const Variable& variable = variables_[v];
    undo_.emplace_back(
        [lent = variable.lent, type = variable.type,
         before = BitsOf(variable.lent.storage, *variable.type)] {
          WriteBits(lent, *type, before);
        });
    WriteBits(variable.lent, *variable.type, bits);
    settled_.insert(variable.lent.storage);
  }
  wanted_.clear();
  Restart();
  return true;
}

void ConstraintProblem::Abandon() {
  for (const std::function<void()>& undo : std::views::reverse(undo_)) {
    undo();
  }
  undo_.clear();
}

auto ConstraintProblem::Named(
    const ErasedReference& lent, const void* value,
    const value::ValueType& type) -> std::int64_t {
  if (const auto it = variable_at_.find(lent.storage);
      it != variable_at_.end()) {
    return variables_[it->second].term;
  }
  return AddVariable(
      lent, value, type, Chosen(RandomQualifier::kNone, false, lent.storage),
      nullptr, MembersOf(lent));
}

auto ConstraintProblem::Constant(
    const void* value, const value::ValueType& type) -> std::int64_t {
  return Make(ConstraintCircuit::Constant(BitsOf(value, type)));
}

auto ConstraintProblem::Negate(std::int64_t a) -> std::int64_t {
  return Make(circuit_.Neg(Bits(a)));
}

auto ConstraintProblem::BitwiseNot(std::int64_t a) -> std::int64_t {
  return Make(ConstraintCircuit::Not(Bits(a)));
}

auto ConstraintProblem::ReduceAnd(std::int64_t a) -> std::int64_t {
  return Make({circuit_.ReduceAnd(Bits(a))});
}

auto ConstraintProblem::ReduceOr(std::int64_t a) -> std::int64_t {
  return Make({circuit_.ReduceOr(Bits(a))});
}

auto ConstraintProblem::ReduceXor(std::int64_t a) -> std::int64_t {
  return Make({circuit_.ReduceXor(Bits(a))});
}

auto ConstraintProblem::LogicalNot(std::int64_t a) -> std::int64_t {
  return Make({Truth(a).Negated()});
}

auto ConstraintProblem::Add(std::int64_t a, std::int64_t b) -> std::int64_t {
  return Make(circuit_.Add(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::Subtract(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make(circuit_.Sub(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::Multiply(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make(circuit_.Mul(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::Divide(std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return Make(circuit_.Div(Bits(a), SameWidth(a, b), is_signed));
}

auto ConstraintProblem::Remainder(
    std::int64_t a, std::int64_t b, bool is_signed) -> std::int64_t {
  return Make(circuit_.Rem(Bits(a), SameWidth(a, b), is_signed));
}

auto ConstraintProblem::BitwiseAnd(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make(circuit_.And(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::BitwiseOr(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make(circuit_.Or(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::BitwiseXor(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make(circuit_.Xor(Bits(a), SameWidth(a, b)));
}

auto ConstraintProblem::ShiftLeft(std::int64_t a, std::int64_t amount)
    -> std::int64_t {
  return Make(circuit_.ShiftLeft(Bits(a), Bits(amount)));
}

auto ConstraintProblem::ShiftRight(
    std::int64_t a, std::int64_t amount, bool arithmetic) -> std::int64_t {
  return Make(circuit_.ShiftRight(Bits(a), Bits(amount), arithmetic));
}

auto ConstraintProblem::Equal(std::int64_t a, std::int64_t b) -> std::int64_t {
  return Make({circuit_.Equal(Bits(a), SameWidth(a, b))});
}

auto ConstraintProblem::Less(std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return Make({circuit_.Less(Bits(a), SameWidth(a, b), is_signed)});
}

auto ConstraintProblem::LogicalAnd(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make({circuit_.And(Truth(a), Truth(b))});
}

auto ConstraintProblem::LogicalOr(std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return Make({circuit_.Or(Truth(a), Truth(b))});
}

auto ConstraintProblem::Concatenate(std::int64_t high, std::int64_t low)
    -> std::int64_t {
  CircuitBits bits = Bits(low);
  const CircuitBits& upper = Bits(high);
  bits.insert(bits.end(), upper.begin(), upper.end());
  return Make(std::move(bits));
}

auto ConstraintProblem::Select(
    std::int64_t condition, std::int64_t then_term, std::int64_t else_term)
    -> std::int64_t {
  return Make(circuit_.Select(
      Truth(condition), Bits(then_term), SameWidth(then_term, else_term)));
}

auto ConstraintProblem::Resize(
    std::int64_t a, std::int64_t width, bool sign_extend) -> std::int64_t {
  return Make(
      ConstraintCircuit::Resize(
          Bits(a), static_cast<std::size_t>(width), sign_extend));
}

auto ConstraintProblem::Slice(
    std::int64_t a, std::int64_t offset, std::int64_t width) -> std::int64_t {
  const CircuitBits& bits = Bits(a);
  CircuitBits out;
  for (std::int64_t i = 0; i < width; ++i) {
    const auto at = static_cast<std::size_t>(offset + i);
    out.push_back(at < bits.size() ? bits[at] : kFalseLiteral);
  }
  return Make(std::move(out));
}

void ConstraintProblem::Require(std::int64_t condition) {
  hard_.push_back(Truth(condition));
}

void ConstraintProblem::Prefer(std::int64_t condition) {
  const CircuitLiteral truth = Truth(condition);
  const std::array roots{truth};
  soft_.push_back(Soft{.condition = truth, .support = circuit_.Support(roots)});
}

auto ConstraintProblem::VariableOf(std::int64_t term) const
    -> std::optional<std::size_t> {
  for (std::size_t i = 0; i < variables_.size(); ++i) {
    if (variables_[i].term == static_cast<std::uint32_t>(term)) {
      return i;
    }
  }
  return std::nullopt;
}

// An ordering, a disabled soft constraint or a wanted variable naming a size
// the call does not choose names a value, which there is nothing to do to.
void ConstraintProblem::DisableSoft(std::int64_t variable) {
  const std::optional<std::size_t> v = VariableOf(variable);
  if (!v.has_value()) {
    return;
  }
  const Variable& named = variables_[*v];
  for (Soft& soft : soft_) {
    for (const std::uint32_t input : named.inputs) {
      if (std::ranges::binary_search(soft.support, input)) {
        soft.disabled = true;
        break;
      }
    }
  }
}

void ConstraintProblem::SolveBefore(std::int64_t before, std::int64_t after) {
  const std::optional<std::size_t> b = VariableOf(before);
  const std::optional<std::size_t> a = VariableOf(after);
  if (b.has_value() && a.has_value()) {
    order_.emplace_back(*b, *a);
  }
}

auto ConstraintProblem::Distribution(
    std::int64_t subject, std::int64_t guard, bool is_signed, bool soft)
    -> std::int64_t {
  distributions_.push_back(
      WeightedChoice{
          .subject = Bits(subject),
          .guard = Truth(guard),
          .is_signed = is_signed,
          .soft = soft,
          .items = {},
          .default_weight = std::nullopt});
  return static_cast<std::int64_t>(distributions_.size() - 1);
}

auto ConstraintProblem::HeldBits(std::int64_t term) const -> std::vector<bool> {
  const CircuitBits& bits = Bits(term);
  std::vector<bool> out;
  out.reserve(bits.size());
  for (const CircuitLiteral bit : bits) {
    if (!bit.IsConstant()) {
      throw SimulationError(
          "a distribution whose weight or range a random variable decides is "
          "not yet supported");
    }
    out.push_back(bit == kTrueLiteral);
  }
  return out;
}

void ConstraintProblem::DistributionItem(
    std::int64_t distribution, std::int64_t low, std::int64_t high,
    std::int64_t weight, bool per_value) {
  WeightedChoice& choice =
      distributions_.at(static_cast<std::size_t>(distribution));
  const CircuitBits& low_bits = Bits(low);
  const CircuitBits& high_bits = Bits(high);
  const CircuitLiteral member = circuit_.And(
      circuit_.Less(choice.subject, low_bits, choice.is_signed).Negated(),
      circuit_.Less(high_bits, choice.subject, choice.is_signed).Negated());
  Natural each = Natural::FromBits(HeldBits(weight));
  if (per_value) {
    // The bounds in the order the range compares them: a signed bound has its
    // sign bit turned over, which orders two's complement values as unsigned
    // ones.
    const auto ordered = [&](std::int64_t bound) {
      std::vector<bool> bits = HeldBits(bound);
      if (choice.is_signed && !bits.empty()) {
        bits.back() = !bits.back();
      }
      return Natural::FromBits(bits);
    };
    const Natural first = ordered(low);
    const Natural last = ordered(high);
    each = last < first ? Natural{} : each * (last - first + Natural{1});
  }
  choice.items.push_back(WeightedValues{.member = member, .weight = each});
}

void ConstraintProblem::DistributionDefault(
    std::int64_t distribution, std::int64_t weight) {
  distributions_.at(static_cast<std::size_t>(distribution)).default_weight =
      Natural::FromBits(HeldBits(weight));
}

auto ConstraintProblem::UniqueSet() -> std::int64_t {
  unique_.emplace_back();
  return static_cast<std::int64_t>(unique_.size() - 1);
}

void ConstraintProblem::UniqueMember(std::int64_t set, std::int64_t term) {
  unique_.at(static_cast<std::size_t>(set)).push_back(term);
}

void ConstraintProblem::UniqueRequire(std::int64_t set, std::int64_t guard) {
  const std::vector<std::int64_t>& members =
      unique_.at(static_cast<std::size_t>(set));
  const CircuitLiteral holds = Truth(guard);
  for (std::size_t i = 0; i < members.size(); ++i) {
    for (std::size_t j = i + 1; j < members.size(); ++j) {
      const CircuitLiteral same =
          circuit_.Equal(Bits(members[i]), SameWidth(members[i], members[j]));
      hard_.push_back(circuit_.Or(holds.Negated(), same.Negated()));
    }
  }
}

auto ConstraintProblem::ChooseDistributions(CircuitLiteral& condition) -> bool {
  ConstraintSampler sampler(circuit_, runtime_->Randomize().NodeBudget());
  for (const WeightedChoice& choice : distributions_) {
    // The values each item names, of nonzero weight, and those no item names
    // where a default weighs them.
    std::vector<WeightedValues> candidates;
    CircuitLiteral named = kFalseLiteral;
    for (const WeightedValues& item : choice.items) {
      named = circuit_.Or(named, item.member);
      if (!item.weight.IsZero()) {
        candidates.push_back(item);
      }
    }
    if (choice.default_weight.has_value() && !choice.default_weight->IsZero()) {
      candidates.push_back(
          WeightedValues{
              .member = named.Negated(), .weight = *choice.default_weight});
    }
    std::optional<CircuitLiteral> taken;
    while (!candidates.empty() && !taken.has_value()) {
      Natural total;
      for (const WeightedValues& candidate : candidates) {
        total = total + candidate.weight;
      }
      Natural point = Natural::DrawBelow(CallGenerator(), total);
      std::size_t picked = 0;
      while (!(point < candidates[picked].weight)) {
        point = point - candidates[picked].weight;
        ++picked;
      }
      const CircuitLiteral held =
          circuit_.Or(choice.guard.Negated(), candidates[picked].member);
      if (sampler.Satisfiable(circuit_.And(condition, held))) {
        taken = held;
      } else {
        candidates.erase(
            candidates.begin() + static_cast<std::ptrdiff_t>(picked));
      }
    }
    if (!taken.has_value()) {
      // No item is left room for: a distribution that must hold cannot,
      // unless its guard is false wherever the constraints hold.
      const CircuitLiteral unguarded = choice.guard.Negated();
      if (choice.soft) {
        continue;
      }
      if (!sampler.Satisfiable(circuit_.And(condition, unguarded))) {
        return false;
      }
      taken = unguarded;
    }
    if (choice.soft) {
      const std::array roots{*taken};
      soft_.push_back(
          Soft{.condition = *taken, .support = circuit_.Support(roots)});
    } else {
      condition = circuit_.And(condition, *taken);
      hard_.push_back(*taken);
    }
  }
  return true;
}

auto ConstraintProblem::CallGenerator() -> DrawRng& {
  if (!call_generator_.has_value()) {
    call_generator_ = generator_->Split();
  }
  return *call_generator_;
}

auto ConstraintProblem::MemberValues(const Variable& variable)
    -> std::vector<std::vector<bool>> {
  std::vector<std::vector<bool>> out;
  if (variable.members.has_value()) {
    for (const std::optional<std::vector<bool>>& member : *variable.members) {
      if (member.has_value()) {
        out.push_back(*member);
      }
    }
  }
  return out;
}

auto ConstraintProblem::Pin(
    const Variable& variable, const std::vector<bool>& bits) -> CircuitLiteral {
  CircuitLiteral pinned = kTrueLiteral;
  for (std::size_t i = 0; i < variable.inputs.size(); ++i) {
    const CircuitLiteral input{circuit_.InputNode(variable.inputs[i]) << 1U};
    pinned = circuit_.And(pinned, bits[i] ? input : input.Negated());
  }
  return pinned;
}

namespace {

// What the constraints `roots` say of the variable whose input bits are
// `own`, as a number two calls agree on exactly where the constraints are the
// same.
auto ConstraintSignature(
    ConstraintCircuit& circuit, std::span<const CircuitLiteral> roots,
    std::span<const std::uint32_t> own) -> std::size_t {
  CircuitLiteral all = kTrueLiteral;
  for (const CircuitLiteral root : roots) {
    all = circuit.And(all, root);
  }
  const CanonicalCondition canonical = CanonicalConditionOf(circuit, all, own);
  std::size_t seed = 0;
  base::HashCombine(seed, canonical.others);
  for (const std::uint32_t word : canonical.words) {
    base::HashCombine(seed, word);
  }
  return seed;
}

}  // namespace

auto ConstraintProblem::ChooseCycles(CircuitLiteral& condition) -> bool {
  ConstraintSampler sampler(circuit_, runtime_->Randomize().NodeBudget());
  // The constraints on each cyclic variable, read before any is pinned: every
  // hard or soft constraint reading one of its bits.
  std::vector<std::uint64_t> signatures(variables_.size(), 0);
  for (std::size_t v = 0; v < variables_.size(); ++v) {
    if (variables_[v].cycle == nullptr) {
      continue;
    }
    const auto reads_it = [&](CircuitLiteral literal) {
      const std::array roots{literal};
      const std::vector<std::uint32_t> support = circuit_.Support(roots);
      return std::ranges::any_of(
          variables_[v].inputs, [&](std::uint32_t input) {
            return std::ranges::binary_search(support, input);
          });
    };
    std::vector<CircuitLiteral> hard_on_it;
    for (const CircuitLiteral hard : hard_) {
      if (reads_it(hard)) {
        hard_on_it.push_back(hard);
      }
    }
    std::vector<CircuitLiteral> soft_on_it;
    for (const Soft& soft : soft_) {
      if (!soft.disabled && reads_it(soft.condition)) {
        soft_on_it.push_back(soft.condition);
      }
    }
    std::size_t signature =
        ConstraintSignature(circuit_, hard_on_it, variables_[v].inputs);
    base::HashCombine(
        signature,
        ConstraintSignature(circuit_, soft_on_it, variables_[v].inputs));
    signatures[v] = signature;
  }
  for (std::size_t v = 0; v < variables_.size(); ++v) {
    const Variable& variable = variables_[v];
    ConstraintCycle* cycle = variable.cycle;
    if (cycle == nullptr) {
      continue;
    }
    cycle->ConstrainedBy(signatures[v], CallGenerator());
    // The value at a position of the cycle: the member at that position for a
    // variable of an enumerated type, which runs through its members, and the
    // position itself for any other.
    const auto bits_at =
        [&](std::uint64_t position) -> std::optional<std::vector<bool>> {
      if (variable.members.has_value()) {
        return (*variable.members)[position];
      }
      std::vector<bool> bits(variable.inputs.size());
      for (std::size_t i = 0; i < bits.size(); ++i) {
        bits[i] = i < 64 && ((position >> i) & 1U) != 0;
      }
      return bits;
    };
    const auto position_of =
        [&](const std::vector<bool>& bits) -> std::uint64_t {
      if (variable.members.has_value()) {
        const auto at = std::ranges::find(*variable.members, bits);
        return static_cast<std::uint64_t>(at - variable.members->begin());
      }
      std::uint64_t position = 0;
      for (std::size_t i = 0; i < bits.size() && i < 64; ++i) {
        position |= std::uint64_t{bits[i] ? 1U : 0U} << i;
      }
      return position;
    };
    const std::array roots{condition};
    const std::vector<std::uint32_t> support = circuit_.Support(roots);
    const bool constrained =
        std::ranges::any_of(variable.inputs, [&](std::uint32_t input) {
          return std::ranges::binary_search(support, input);
        });
    // A variable no constraint reaches takes the permutation's next position.
    // One a constraint reaches takes a position drawn uniformly among those
    // the constraints allow and this permutation has not yet taken, which is a
    // random permutation of the allowed values (LRM 18.4.2).
    const auto choose = [&]() -> std::optional<std::uint64_t> {
      if (!constrained) {
        return cycle->ValueAt(cycle->Cursor());
      }
      CircuitLiteral untaken = condition;
      for (const std::uint64_t taken : cycle->Taken()) {
        if (const std::optional<std::vector<bool>> bits = bits_at(taken)) {
          untaken = circuit_.And(untaken, Pin(variable, *bits).Negated());
        }
      }
      const std::vector<std::vector<bool>> members = MemberValues(variable);
      const std::array one{
          DrawnVariable{.inputs = variable.inputs, .members = members}};
      const std::optional<std::vector<bool>> drawn =
          sampler.Draw(untaken, one, CallGenerator());
      drew_past_budget_ = drew_past_budget_ || sampler.DrewPastBudget();
      if (!drawn.has_value()) {
        return std::nullopt;
      }
      return position_of(*drawn);
    };
    std::optional<std::uint64_t> value = choose();
    if (!value.has_value()) {
      // None left in this permutation meets the constraints, so another is
      // begun (LRM 18.4.2).
      cycle->Restart(CallGenerator());
      value = choose();
    }
    if (!value.has_value()) {
      return false;
    }
    const CircuitLiteral pinned = Pin(variable, *bits_at(*value));
    condition = circuit_.And(condition, pinned);
    hard_.push_back(pinned);
    cycled_.emplace_back(cycle, *value);
  }
  return true;
}

void ConstraintProblem::AddNested(const value::ObjectRef& nested) {
  GcObject* root = ObjectRootOf(nested);
  if (root == ObjectRootOf(object_)) {
    return;
  }
  for (const value::ObjectRef& known : nested_) {
    if (ObjectRootOf(known) == root) {
      return;
    }
  }
  nested_.push_back(nested);
}

auto ConstraintProblem::Solve() -> bool {
  answer_.assign(circuit_.InputCount(), false);
  cycled_.clear();
  CircuitLiteral everything = kTrueLiteral;
  for (const CircuitLiteral condition : hard_) {
    everything = circuit_.And(everything, condition);
  }
  if (!ChooseCycles(everything)) {
    return false;
  }
  if (!ChooseDistributions(everything)) {
    return false;
  }

  // Which chosen variable owns each input, to group what constrains what.
  std::vector<std::int64_t> owner(circuit_.InputCount(), -1);
  for (std::size_t v = 0; v < variables_.size(); ++v) {
    for (const std::uint32_t input : variables_[v].inputs) {
      owner[input] = static_cast<std::int64_t>(v);
    }
  }
  std::vector<std::size_t> group(variables_.size());
  std::ranges::iota(group, std::size_t{0});
  const auto find = [&](std::size_t v) {
    while (group[v] != v) {
      group[v] = group[group[v]];
      v = group[v];
    }
    return v;
  };
  const auto join = [&](std::span<const std::uint32_t> support) {
    std::optional<std::size_t> first;
    for (const std::uint32_t input : support) {
      if (owner[input] < 0) {
        continue;
      }
      const std::size_t root = find(static_cast<std::size_t>(owner[input]));
      if (!first.has_value()) {
        first = root;
      } else {
        group[root] = *first;
      }
    }
  };
  std::vector<std::vector<std::uint32_t>> hard_support;
  for (const CircuitLiteral condition : hard_) {
    const std::array roots{condition};
    hard_support.push_back(circuit_.Support(roots));
    join(hard_support.back());
  }
  for (const Soft& soft : soft_) {
    if (!soft.disabled) {
      join(soft.support);
    }
  }
  for (const auto& [before, after] : order_) {
    group[find(before)] = find(after);
  }

  // Independent groups are drawn apart: a uniform draw over a product is a
  // uniform draw of each factor.
  std::vector<std::vector<std::size_t>> members(variables_.size());
  for (std::size_t v = 0; v < variables_.size(); ++v) {
    if (variables_[v].random) {
      members[find(v)].push_back(v);
    }
  }
  const auto group_of_support = [&](std::span<const std::uint32_t> support)
      -> std::optional<std::size_t> {
    for (const std::uint32_t input : support) {
      if (owner[input] >= 0) {
        return find(static_cast<std::size_t>(owner[input]));
      }
    }
    return std::nullopt;
  };
  std::vector<CircuitLiteral> condition_of(variables_.size(), kTrueLiteral);
  for (std::size_t c = 0; c < hard_.size(); ++c) {
    const std::optional<std::size_t> g = group_of_support(hard_support[c]);
    if (!g.has_value()) {
      // A constraint over held values alone is a check (LRM 18.11.1).
      if (hard_[c] != kTrueLiteral && !circuit_.Evaluate(hard_[c], answer_)) {
        return false;
      }
      continue;
    }
    condition_of[*g] = circuit_.And(condition_of[*g], hard_[c]);
  }

  for (std::size_t g = 0; g < variables_.size(); ++g) {
    const std::vector<std::size_t>& vars = members[g];
    if (vars.empty()) {
      continue;
    }
    ConstraintSampler sampler(circuit_, runtime_->Randomize().NodeBudget());
    CircuitLiteral condition = condition_of[g];

    for (std::size_t s = soft_.size(); s-- > 0;) {
      const Soft& soft = soft_[s];
      if (soft.disabled || group_of_support(soft.support) != g) {
        continue;
      }
      const CircuitLiteral with = circuit_.And(condition, soft.condition);
      if (sampler.Satisfiable(with)) {
        condition = with;
      }
    }

    // Each variable's place in the ordering: how many orderings lead to it.
    // One no ordering names is chosen with the last (LRM 18.5.9).
    std::vector<std::size_t> depth(variables_.size(), 0);
    bool ordered = false;
    for (std::size_t pass = 0; pass < variables_.size(); ++pass) {
      for (const auto& [before, after] : order_) {
        if (find(before) != g) {
          continue;
        }
        ordered = true;
        depth[after] = std::max(depth[after], depth[before] + 1);
      }
    }
    std::size_t last = 0;
    for (const std::size_t v : vars) {
      last = std::max(last, depth[v]);
    }
    std::vector<bool> named(variables_.size(), false);
    for (const auto& [before, after] : order_) {
      named[before] = true;
      named[after] = true;
    }
    for (std::size_t layer = 0; layer <= last; ++layer) {
      // The layer's variables in the order they were declared, which is the
      // order their bits are decided in.
      std::vector<std::size_t> at_layer;
      for (const std::size_t v : vars) {
        const std::size_t at = ordered && named[v] ? depth[v] : last;
        if (at == layer) {
          at_layer.push_back(v);
        }
      }
      if (at_layer.empty()) {
        continue;
      }
      std::vector<std::vector<std::vector<bool>>> allowed;
      allowed.reserve(at_layer.size());
      std::vector<DrawnVariable> drawn;
      std::vector<std::uint32_t> inputs;
      for (const std::size_t v : at_layer) {
        allowed.push_back(MemberValues(variables_[v]));
        drawn.push_back(
            DrawnVariable{
                .inputs = variables_[v].inputs, .members = allowed.back()});
        inputs.insert(
            inputs.end(), variables_[v].inputs.begin(),
            variables_[v].inputs.end());
      }
      const std::optional<std::vector<bool>> value =
          sampler.Draw(condition, drawn, CallGenerator());
      drew_past_budget_ = drew_past_budget_ || sampler.DrewPastBudget();
      if (!value.has_value()) {
        return false;
      }
      for (std::size_t i = 0; i < inputs.size(); ++i) {
        answer_[inputs[i]] = (*value)[i];
        const CircuitLiteral input{circuit_.InputNode(inputs[i]) << 1U};
        condition =
            circuit_.And(condition, (*value)[i] ? input : input.Negated());
      }
    }
  }
  // Every answer meets the constraints, but one drawn past what the run can
  // count is not drawn uniformly (LRM 18.5.9), which the run says once.
  if (drew_past_budget_ && runtime_->Randomize().FirstPastBudget()) {
    const std::array notes{base::DiagMessage(
        "raise the limit with '+lyra+randomize+nodes=<count>'")};
    runtime_->Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "'randomize' drew values without a uniform distribution; its "
            "constraints have too many solutions to count"),
        notes);
  }
  return true;
}

void ConstraintProblem::Commit() {
  for (const Variable& variable : variables_) {
    if (!variable.random || variable.size_of.has_value()) {
      continue;
    }
    std::vector<bool> bits(variable.inputs.size());
    for (std::size_t i = 0; i < bits.size(); ++i) {
      bits[i] = answer_[variable.inputs[i]];
    }
    WriteBits(variable.lent, *variable.type, bits);
  }
  for (const auto& [cycle, value] : cycled_) {
    cycle->Take(value, CallGenerator());
  }
}

}  // namespace lyra::runtime
