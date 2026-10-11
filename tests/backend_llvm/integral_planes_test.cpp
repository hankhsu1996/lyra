#include "lyra/backend/llvm/integral_planes.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <gtest/gtest.h>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/Analysis/TargetFolder.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/base/overloaded.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::backend::llvm_backend {
namespace {

using support::IntegralOperandKind;
using support::IntegralOperation;
using value::IntegralShape;
using value::Signedness;
using value::StateDomain;

// The words of one plane, least significant first.
using Words = std::vector<std::uint64_t>;

auto WordsOf(const llvm::APInt& bits) -> Words {
  return {bits.getRawData(), bits.getRawData() + bits.getNumWords()};
}

// One operand as an application hands it over: the two planes of an integral
// value, or a machine value as the one word of `value`.
struct Sample {
  Words value;
  Words unknown;

  auto operator==(const Sample&) const -> bool = default;
};

auto MachineSample(std::uint64_t value) -> Sample {
  return Sample{.value = {value}, .unknown = {0}};
}

// The planes spelled most significant word first.
auto Spelled(const Words& words) -> std::string {
  std::string spelled = "0x";
  for (auto word = words.rbegin(); word != words.rend(); ++word) {
    spelled += std::format("{:016x}", *word);
  }
  return spelled;
}

auto ShapesOver(
    std::span<const std::uint64_t> widths,
    std::span<const Signedness> signednesses,
    std::span<const StateDomain> domains) -> std::vector<IntegralShape> {
  std::vector<IntegralShape> shapes;
  for (const std::uint64_t width : widths) {
    for (const Signedness signedness : signednesses) {
      for (const StateDomain domain : domains) {
        shapes.push_back(
            IntegralShape{
                .width = width, .signedness = signedness, .domain = domain});
      }
    }
  }
  return shapes;
}

constexpr std::array<std::uint64_t, 8> kWidths{1, 7, 8, 33, 63, 64, 65, 130};
constexpr std::array kBothSignednesses{
    Signedness::kUnsigned, Signedness::kSigned};
constexpr std::array kUnsignedOnly{Signedness::kUnsigned};
constexpr std::array kBothDomains{
    StateDomain::kTwoState, StateDomain::kFourState};

// Alternating runs of set and clear bits over `bits` positions.
auto MixedPattern(unsigned bits) -> llvm::APInt {
  return llvm::APInt::getSplat(std::max(bits, 8U), llvm::APInt(8, 0xA5))
      .trunc(bits);
}

// The values an operand of type `shape` is tried at: 0, 1, every bit set, the
// top bit alone and beside others, the largest number below it, a mixed
// pattern, and -- where the type holds them -- an x and a z in the lowest, the
// highest and a middle position of that pattern, all x, and all z.
auto SamplesOf(const IntegralShape& shape) -> std::vector<Sample> {
  const auto bits = static_cast<unsigned>(shape.width);
  const llvm::APInt zero(bits, 0);
  const llvm::APInt one(bits, 1);
  const llvm::APInt mask = llvm::APInt::getAllOnes(bits);
  const llvm::APInt top = llvm::APInt::getOneBitSet(bits, bits - 1U);
  const llvm::APInt middle = llvm::APInt::getOneBitSet(bits, bits / 2U);
  const llvm::APInt pattern = MixedPattern(bits);
  std::vector<Sample> samples;
  const auto add = [&](const llvm::APInt& value, const llvm::APInt& unknown) {
    Sample sample{.value = WordsOf(value), .unknown = WordsOf(unknown)};
    if (std::ranges::find(samples, sample) == samples.end()) {
      samples.push_back(std::move(sample));
    }
  };
  for (const llvm::APInt& known :
       {zero, one, mask, top, top | one, mask ^ top, pattern}) {
    add(known, zero);
  }
  if (shape.IsFourState()) {
    for (const llvm::APInt& bit : {one, top, middle}) {
      add(pattern | bit, bit);
      add(pattern & ~bit, bit);
    }
    add(mask, mask);
    add(zero, mask);
  }
  return samples;
}

// A thinner set for the last operand of an operation taking three, whose
// applications otherwise multiply past what a test can run.
auto FewSamplesOf(const IntegralShape& shape) -> std::vector<Sample> {
  const auto bits = static_cast<unsigned>(shape.width);
  const llvm::APInt zero(bits, 0);
  const llvm::APInt one(bits, 1);
  const llvm::APInt top = llvm::APInt::getOneBitSet(bits, bits - 1U);
  const llvm::APInt pattern = MixedPattern(bits);
  const auto sample = [](const llvm::APInt& value, const llvm::APInt& unknown) {
    return Sample{.value = WordsOf(value), .unknown = WordsOf(unknown)};
  };
  std::vector<Sample> samples{
      sample(zero, zero), sample(llvm::APInt::getAllOnes(bits), zero),
      sample(pattern, zero)};
  if (shape.IsFourState()) {
    samples.push_back(sample(pattern | one, one));
    samples.push_back(sample(pattern & ~top, top));
  }
  return samples;
}

// The types operand `index` of an operation is tried at, given the types the
// operands before it were. An operand read as bits is one no operation reads
// the signedness of, so it is tried at one; an operand after the first is
// tried at fewer types.
auto TypesOf(
    IntegralOperandKind kind, std::size_t index,
    std::span<const std::optional<IntegralShape>> before)
    -> std::vector<std::optional<IntegralShape>> {
  constexpr std::array<std::uint64_t, 4> kPositionWidths{7, 33, 64, 100};
  constexpr std::array<std::uint64_t, 4> kLastWidths{1, 7, 33, 70};
  const auto each = [](const std::vector<IntegralShape>& shapes) {
    return std::vector<std::optional<IntegralShape>>(
        shapes.begin(), shapes.end());
  };
  switch (kind) {
    case IntegralOperandKind::kBits:
      return each(ShapesOver(
          index < 2 ? std::span<const std::uint64_t>{kWidths}
                    : std::span<const std::uint64_t>{kLastWidths},
          kUnsignedOnly, kBothDomains));
    case IntegralOperandKind::kNumber: {
      if (index == 0) {
        return each(ShapesOver(kWidths, kBothSignednesses, kBothDomains));
      }
      std::vector<IntegralShape> shapes = ShapesOver(
          kPositionWidths, kBothSignednesses,
          std::array{StateDomain::kTwoState});
      shapes.push_back(
          IntegralShape{
              .width = 7,
              .signedness = Signedness::kSigned,
              .domain = StateDomain::kFourState});
      shapes.push_back(
          IntegralShape{
              .width = 64,
              .signedness = Signedness::kUnsigned,
              .domain = StateDomain::kFourState});
      return each(shapes);
    }
    case IntegralOperandKind::kSameType:
      return {before.front()};
    case IntegralOperandKind::kMachineInt:
    case IntegralOperandKind::kMachineBool:
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return {std::nullopt};
  }
  return {};
}

// Whether this test can make an operand of `kind`. Text and the buffers of a
// foreign call are no planes.
auto CanMake(IntegralOperandKind kind) -> bool {
  switch (kind) {
    case IntegralOperandKind::kBits:
    case IntegralOperandKind::kNumber:
    case IntegralOperandKind::kSameType:
    case IntegralOperandKind::kMachineInt:
    case IntegralOperandKind::kMachineBool:
      return true;
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return false;
  }
  return false;
}

auto ValuesOf(
    IntegralOperandKind kind, std::size_t index,
    const std::optional<IntegralShape>& shape) -> std::vector<Sample> {
  switch (kind) {
    case IntegralOperandKind::kBits:
    case IntegralOperandKind::kNumber:
    case IntegralOperandKind::kSameType:
      return index < 2 ? SamplesOf(*shape) : FewSamplesOf(*shape);
    case IntegralOperandKind::kMachineInt:
      return {
          MachineSample(0),
          MachineSample(1),
          MachineSample(~std::uint64_t{0}),
          MachineSample(std::uint64_t{1} << 63U),
          MachineSample(~std::uint64_t{0} >> 1U),
          MachineSample(std::uint64_t{1} << 32U),
          MachineSample(0xA5A5'A5A5'A5A5'A5A5U)};
    case IntegralOperandKind::kMachineBool:
      return {MachineSample(0), MachineSample(1)};
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return {};
  }
  return {};
}

// A module with one open block, and a builder that folds every instruction
// over constants as it is asked for it, an intrinsic included.
struct Folding {
  llvm::LLVMContext context;
  llvm::Module module{"planes", context};
  llvm::BasicBlock* block = llvm::BasicBlock::Create(
      context, "entry",
      llvm::Function::Create(
          llvm::FunctionType::get(llvm::Type::getVoidTy(context), false),
          llvm::Function::ExternalLinkage, "probe", module));
  llvm::IRBuilder<llvm::TargetFolder> builder{
      block, llvm::TargetFolder(module.getDataLayout())};
};

// The words a folded plane came out as, or nothing where what was built is no
// constant.
auto FoldedWords(const llvm::Value* folded) -> std::optional<Words> {
  const auto* constant = llvm::dyn_cast<llvm::ConstantInt>(folded);
  if (constant == nullptr) {
    return std::nullopt;
  }
  return WordsOf(constant->getValue());
}

// The word a folded machine answer came out as, likewise.
auto FoldedWord(const llvm::Value* folded) -> std::optional<std::uint64_t> {
  const auto* constant = llvm::dyn_cast<llvm::ConstantInt>(folded);
  if (constant == nullptr) {
    return std::nullopt;
  }
  return constant->getZExtValue();
}

struct Answer {
  Words value;
  Words unknown;

  auto operator==(const Answer&) const -> bool = default;
};

auto Spelled(const std::optional<IntegralShape>& shape, const Sample& sample)
    -> std::string {
  if (!shape.has_value()) {
    return std::format("machine {:#x}", sample.value.front());
  }
  return std::format(
      "{}{}{} value {} unknown {}", shape->IsFourState() ? "logic" : "bit",
      shape->signedness == Signedness::kSigned ? " signed " : " ", shape->width,
      Spelled(sample.value), Spelled(sample.unknown));
}

// One operation at one set of operand and answer types, over every
// combination of its operands' values: the instructions the code generator
// builds for it, folded over those values, against the library's statement of
// the same operation over the same values.
class Application {
 public:
  Application(
      Folding& folding, const IntegralOperation& operation,
      IntegralShapes shapes)
      : folding_(&folding),
        operation_(&operation),
        shapes_(std::move(shapes)),
        chosen_(shapes_.operands.size()) {
  }

  // How many combinations were compared, or nothing where the code generator
  // builds no instructions for the operation.
  auto Compare() -> std::optional<std::size_t> {
    compared_ = 0;
    has_arm_ = true;
    Choose(0);
    return has_arm_ ? std::optional{compared_} : std::nullopt;
  }

 private:
  void Choose(std::size_t index) {
    const std::span<const IntegralOperandKind> kinds =
        operation_->operands.Kinds();
    if (index == kinds.size()) {
      CompareChosen();
      return;
    }
    for (const Sample& sample :
         ValuesOf(kinds[index], index, shapes_.operands[index])) {
      if (!has_arm_ || ::testing::Test::HasFailure()) {
        return;
      }
      chosen_[index] = sample;
      Choose(index + 1);
    }
  }

  [[nodiscard]] auto LibraryOperand(std::size_t index) const
      -> value::IntegralOperand {
    const std::span<const IntegralOperandKind> kinds =
        operation_->operands.Kinds();
    const Sample& sample = chosen_[index];
    switch (kinds[index]) {
      case IntegralOperandKind::kBits:
      case IntegralOperandKind::kNumber:
      case IntegralOperandKind::kSameType:
        return value::ConstIntegralView{
            .value = sample.value,
            .unknown = shapes_.operands[index]->IsFourState()
                           ? std::span<const std::uint64_t>{sample.unknown}
                           : std::span<const std::uint64_t>{},
            .width = shapes_.operands[index]->width,
            .signedness = shapes_.operands[index]->signedness};
      case IntegralOperandKind::kMachineInt:
        return static_cast<std::int64_t>(sample.value.front());
      case IntegralOperandKind::kMachineBool:
        return sample.value.front() != 0;
      case IntegralOperandKind::kSvLogic:
      case IntegralOperandKind::kText:
      case IntegralOperandKind::kCanonicalBits:
      case IntegralOperandKind::kCanonicalLogic:
        break;
    }
    return std::int64_t{0};
  }

  void CompareChosen() {
    llvm::IRBuilderBase& builder = folding_->builder;
    const auto planes = [&](std::size_t index) {
      const auto bits = static_cast<unsigned>(shapes_.operands[index]->width);
      llvm::IntegerType* const exact = builder.getIntNTy(bits);
      const auto plane = [&](const Words& words) {
        return llvm::ConstantInt::get(exact, llvm::APInt(bits, words));
      };
      return Planes{
          .value = plane(chosen_[index].value),
          .unknown = plane(chosen_[index].unknown)};
    };
    const auto machine = [&](std::size_t index) -> llvm::Value* {
      switch (operation_->operands.Kinds()[index]) {
        case IntegralOperandKind::kMachineBool:
          return builder.getInt1(chosen_[index].value.front() != 0);
        case IntegralOperandKind::kMachineInt:
        case IntegralOperandKind::kBits:
        case IntegralOperandKind::kNumber:
        case IntegralOperandKind::kSameType:
        case IntegralOperandKind::kSvLogic:
        case IntegralOperandKind::kText:
        case IntegralOperandKind::kCanonicalBits:
        case IntegralOperandKind::kCanonicalLogic:
          break;
      }
      return builder.getInt64(chosen_[index].value.front());
    };
    const std::optional<PlaneAnswer> built = LowerOnPlanes(
        builder, operation_->op, shapes_,
        PlaneOperands{.planes = planes, .machine = machine});
    if (!built.has_value()) {
      has_arm_ = false;
      return;
    }

    std::vector<value::IntegralOperand> operands;
    operands.reserve(chosen_.size());
    for (std::size_t index = 0; index < chosen_.size(); ++index) {
      operands.push_back(LibraryOperand(index));
    }
    const bool answers_unknown =
        shapes_.answer.has_value() && shapes_.answer->IsFourState();
    const std::uint64_t answer_width =
        shapes_.answer.has_value() ? shapes_.answer->width : 0;
    const std::size_t answer_words =
        std::max<std::size_t>(value::WordCountForBits(answer_width), 1);
    Answer stated{
        .value = Words(answer_words, 0), .unknown = Words(answer_words, 0)};
    const value::MachineAnswer stated_machine = value::ApplyIntegralOperation(
        operation_->op, operands,
        value::IntegralView{
            .value = stated.value,
            .unknown = answers_unknown
                           ? std::span<std::uint64_t>{stated.unknown}
                           : std::span<std::uint64_t>{},
            .width = answer_width});

    const auto disagrees = [&](const std::string& how) {
      std::string operands_spelled;
      for (std::size_t index = 0; index < chosen_.size(); ++index) {
        operands_spelled += std::format(
            "\n  operand {}: {}", index,
            Spelled(shapes_.operands[index], chosen_[index]));
      }
      ADD_FAILURE() << operation_->name << " " << how << operands_spelled
                    << (shapes_.answer.has_value()
                            ? std::format(
                                  "\n  answering at {} bits, {}",
                                  shapes_.answer->width,
                                  answers_unknown ? "four-state" : "two-state")
                            : std::string{});
    };
    std::visit(
        Overloaded{
            [&](const Planes& answer) {
              const std::optional<Words> value = FoldedWords(answer.value);
              const std::optional<Words> unknown = FoldedWords(answer.unknown);
              if (!value.has_value() || !unknown.has_value()) {
                disagrees("builds an answer that does not fold to constants");
                return;
              }
              if (!std::holds_alternative<std::monostate>(stated_machine)) {
                disagrees("answers planes where the library answers a value");
                return;
              }
              if (answer.value->getType()->getIntegerBitWidth() !=
                  shapes_.answer->width) {
                disagrees("answers at another width than its answer's type");
                return;
              }
              // An answer that holds no x or z has no unknown plane to lay
              // out, so only its value plane is what the operation answers.
              const Answer inlined{
                  .value = *value,
                  .unknown =
                      answers_unknown ? *unknown : Words(answer_words, 0)};
              if (inlined != stated) {
                disagrees(
                    std::format(
                        "answers value {} unknown {} on planes where the "
                        "library answers value {} unknown {}",
                        Spelled(inlined.value), Spelled(inlined.unknown),
                        Spelled(stated.value), Spelled(stated.unknown)));
              }
            },
            [&](llvm::Value* answer) {
              const std::optional<std::uint64_t> word = FoldedWord(answer);
              if (!word.has_value()) {
                disagrees("builds an answer that does not fold to a constant");
                return;
              }
              const std::optional<std::uint64_t> stated_word = std::visit(
                  Overloaded{
                      [](std::monostate) -> std::optional<std::uint64_t> {
                        return std::nullopt;
                      },
                      [](bool holds) -> std::optional<std::uint64_t> {
                        return holds ? 1U : 0U;
                      },
                      [](std::int64_t number) -> std::optional<std::uint64_t> {
                        return static_cast<std::uint64_t>(number);
                      }},
                  stated_machine);
              if (word != stated_word) {
                disagrees(
                    std::format(
                        "answers the machine value {:#x} on planes where "
                        "the library answers {}",
                        *word,
                        stated_word.has_value()
                            ? std::format("{:#x}", *stated_word)
                            : std::string("planes")));
              }
            }},
        *built);
    if (!folding_->block->empty()) {
      disagrees("leaves instructions a folding builder did not fold");
    }
    ++compared_;
  }

  Folding* folding_;
  const IntegralOperation* operation_;
  IntegralShapes shapes_;
  std::vector<Sample> chosen_;
  std::size_t compared_ = 0;
  bool has_arm_ = true;
};

// Every application of `operation` this test makes: each combination of the
// types its operands are tried at, with each type its answer can then be.
auto ApplicationsOf(const IntegralOperation& operation)
    -> std::vector<IntegralShapes> {
  const std::span<const IntegralOperandKind> kinds = operation.operands.Kinds();
  std::vector<IntegralShapes> applications;
  std::vector<std::optional<IntegralShape>> operands;
  const auto with_answers = [&] {
    std::vector<value::IntegralExtent> extents;
    for (const std::optional<IntegralShape>& operand : operands) {
      if (operand.has_value()) {
        extents.push_back(value::ExtentOf(*operand));
      }
    }
    const auto answering = [&](std::optional<IntegralShape> answer) {
      applications.push_back(
          IntegralShapes{.operands = operands, .answer = answer});
    };
    std::visit(
        Overloaded{
            [&](const value::IntegralExtent& fixed) {
              answering(
                  IntegralShape{
                      .width = fixed.width,
                      .signedness = Signedness::kUnsigned,
                      .domain = fixed.domain});
            },
            [&](const value::AnswerOfTheCall&) {
              for (const IntegralShape& answer :
                   ShapesOver(kWidths, kUnsignedOnly, kBothDomains)) {
                answering(answer);
              }
            },
            [&](const value::MachineBoolAnswer&) { answering(std::nullopt); },
            [&](const value::MachineIntAnswer&) { answering(std::nullopt); }},
        value::AnswerTypeOf(operation.op, extents));
  };
  const auto choose = [&](const auto& self, std::size_t index) -> void {
    if (index == kinds.size()) {
      with_answers();
      return;
    }
    for (const std::optional<IntegralShape>& type :
         TypesOf(kinds[index], index, operands)) {
      // Bits written into a value are no wider than the value.
      if (index == 2 && type.has_value() && operands.front().has_value() &&
          type->width > operands.front()->width) {
        continue;
      }
      operands.push_back(type);
      self(self, index + 1);
      operands.pop_back();
    }
  };
  choose(choose, 0);
  return applications;
}

// The operations whose work grows with the square of the width, which are
// instructions up to one machine word and calls past it.
constexpr std::array<std::string_view, 3> kOnOneWordOnly{"mul", "div", "mod"};

auto PastOneWord(const IntegralShapes& shapes) -> bool {
  const auto past = [](const std::optional<IntegralShape>& shape) {
    return shape.has_value() && !IsSingleWord(*shape);
  };
  return std::ranges::any_of(shapes.operands, past) || past(shapes.answer);
}

// The two statements of an operation -- the instructions the code generator
// builds for it on planes and the library's function over views -- answer
// alike, bit for bit in both planes, at widths on both sides of a machine
// word, for every operation the code generator builds instructions for.
TEST(IntegralPlanesTest, EveryInlineOperationAnswersAsTheLibraryStatesIt) {
  std::set<std::string_view> without_arm;
  std::size_t compared = 0;
  for (const IntegralOperation& operation : support::IntegralOperations()) {
    Folding folding;
    if (!std::ranges::all_of(operation.operands.Kinds(), CanMake)) {
      // No operand is read before the operation is found to have no arm.
      const auto unread = [](std::size_t) -> Planes {
        ADD_FAILURE() << "an operand this test cannot make was read";
        return Planes{.value = nullptr, .unknown = nullptr};
      };
      const auto unread_machine = [](std::size_t) -> llvm::Value* {
        ADD_FAILURE() << "an operand this test cannot make was read";
        return nullptr;
      };
      EXPECT_FALSE(
          LowerOnPlanes(
              folding.builder, operation.op,
              IntegralShapes{
                  .operands = {std::nullopt},
                  .answer =
                      IntegralShape{
                          .width = 8,
                          .signedness = Signedness::kUnsigned,
                          .domain = StateDomain::kFourState}},
              PlaneOperands{.planes = unread, .machine = unread_machine})
              .has_value())
          << operation.name;
      without_arm.insert(operation.name);
      continue;
    }
    std::size_t compared_here = 0;
    bool has_arm = true;
    const bool one_word_only =
        std::ranges::find(kOnOneWordOnly, operation.name) !=
        kOnOneWordOnly.end();
    for (IntegralShapes& shapes : ApplicationsOf(operation)) {
      const bool past_one_word = PastOneWord(shapes);
      const std::optional<std::size_t> count =
          Application(folding, operation, std::move(shapes)).Compare();
      if (one_word_only && past_one_word) {
        EXPECT_FALSE(count.has_value())
            << operation.name << " is built on planes past one machine word";
        continue;
      }
      if (!count.has_value()) {
        has_arm = false;
        continue;
      }
      compared_here += *count;
      if (HasFailure()) {
        return;
      }
    }
    if (!has_arm) {
      EXPECT_EQ(compared_here, 0U)
          << operation.name
          << " is built on planes for some types and not others";
      without_arm.insert(operation.name);
      continue;
    }
    EXPECT_GT(compared_here, 0U) << operation.name << " was never compared";
    compared += compared_here;
  }
  // The operations whose work is long at every width, which are calls.
  EXPECT_EQ(
      without_arm,
      (std::set<std::string_view>{
          "pow", "count_bits", "clog2", "reverse_blocks", "from_text",
          "read_canonical_bits", "read_canonical_logic", "from_sv_logic"}));
  EXPECT_GT(compared, 100000U);
}

}  // namespace
}  // namespace lyra::backend::llvm_backend
