#include "lyra/backend/llvm/integral_one_word.hpp"

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

// One operand as an application hands it over: the two plane words of an
// integral value, or a machine value in `value`.
struct Sample {
  std::uint64_t value = 0;
  std::uint64_t unknown = 0;

  auto operator==(const Sample&) const -> bool = default;
};

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

constexpr std::array<std::uint64_t, 6> kWidths{1, 7, 8, 33, 63, 64};
constexpr std::array kBothSignednesses{
    Signedness::kUnsigned, Signedness::kSigned};
constexpr std::array kUnsignedOnly{Signedness::kUnsigned};
constexpr std::array kBothDomains{
    StateDomain::kTwoState, StateDomain::kFourState};

// The values an operand of type `shape` is tried at: 0, 1, every bit set, the
// top bit alone and beside others, the largest number below it, a mixed
// pattern, and -- where the type holds them -- an x and a z in the lowest, the
// highest and a middle position of that pattern, all x, and all z.
auto SamplesOf(const IntegralShape& shape) -> std::vector<Sample> {
  const std::uint64_t mask = value::LowBits(shape.width);
  const std::uint64_t top = std::uint64_t{1} << (shape.width - 1U);
  const std::uint64_t middle = std::uint64_t{1} << (shape.width / 2U);
  const std::uint64_t pattern = 0xA5A5'A5A5'A5A5'A5A5U & mask;
  std::vector<Sample> samples;
  const auto add = [&](Sample sample) {
    if (std::ranges::find(samples, sample) == samples.end()) {
      samples.push_back(sample);
    }
  };
  for (const std::uint64_t known : std::array<std::uint64_t, 7>{
           0, 1, mask, top, top | 1U, mask ^ top, pattern}) {
    add(Sample{.value = known & mask});
  }
  if (shape.IsFourState()) {
    for (const std::uint64_t bit : {std::uint64_t{1}, top, middle}) {
      add(Sample{.value = pattern | bit, .unknown = bit});
      add(Sample{.value = pattern & ~bit, .unknown = bit});
    }
    add(Sample{.value = mask, .unknown = mask});
    add(Sample{.value = 0, .unknown = mask});
  }
  return samples;
}

// A thinner set for the last operand of an operation taking three, whose
// applications otherwise multiply past what a test can run.
auto FewSamplesOf(const IntegralShape& shape) -> std::vector<Sample> {
  const std::uint64_t mask = value::LowBits(shape.width);
  const std::uint64_t top = std::uint64_t{1} << (shape.width - 1U);
  const std::uint64_t pattern = 0xA5A5'A5A5'A5A5'A5A5U & mask;
  std::vector<Sample> samples{
      Sample{.value = 0}, Sample{.value = mask}, Sample{.value = pattern}};
  if (shape.IsFourState()) {
    samples.push_back(Sample{.value = pattern | 1U, .unknown = 1});
    samples.push_back(Sample{.value = pattern & ~top, .unknown = top});
  }
  return samples;
}

// The types operand `index` of an operation is tried at, given the types the
// operands before it were. An operand read as bits is told no signedness, so
// it is tried at one; an operand after the first is tried at fewer types.
auto TypesOf(
    IntegralOperandKind kind, std::size_t index,
    std::span<const std::optional<IntegralShape>> before)
    -> std::vector<std::optional<IntegralShape>> {
  constexpr std::array<std::uint64_t, 3> kPositionWidths{7, 33, 64};
  constexpr std::array<std::uint64_t, 3> kLastWidths{1, 7, 33};
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
// foreign call are no values on one word.
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
          Sample{.value = 0},
          Sample{.value = 1},
          Sample{.value = ~std::uint64_t{0}},
          Sample{.value = std::uint64_t{1} << 63U},
          Sample{.value = ~std::uint64_t{0} >> 1U},
          Sample{.value = std::uint64_t{1} << 32U},
          Sample{.value = 0xA5A5'A5A5'A5A5'A5A5U}};
    case IntegralOperandKind::kMachineBool:
      return {Sample{.value = 0}, Sample{.value = 1}};
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
  llvm::Module module{"one_word", context};
  llvm::BasicBlock* block = llvm::BasicBlock::Create(
      context, "entry",
      llvm::Function::Create(
          llvm::FunctionType::get(llvm::Type::getVoidTy(context), false),
          llvm::Function::ExternalLinkage, "probe", module));
  llvm::IRBuilder<llvm::TargetFolder> builder{
      block, llvm::TargetFolder(module.getDataLayout())};
};

// The word a folded plane or machine answer came out as, or nothing where
// what was built is no constant.
auto WordOf(const llvm::Value* folded) -> std::optional<std::uint64_t> {
  const auto* constant = llvm::dyn_cast<llvm::ConstantInt>(folded);
  if (constant == nullptr) {
    return std::nullopt;
  }
  return constant->getZExtValue();
}

struct Answer {
  std::uint64_t value = 0;
  std::uint64_t unknown = 0;

  auto operator==(const Answer&) const -> bool = default;
};

auto Spelled(const std::optional<IntegralShape>& shape, const Sample& sample)
    -> std::string {
  if (!shape.has_value()) {
    return std::format("machine {:#x}", sample.value);
  }
  return std::format(
      "{}{}{} value {:#x} unknown {:#x}",
      shape->IsFourState() ? "logic" : "bit",
      shape->signedness == Signedness::kSigned ? " signed " : " ", shape->width,
      sample.value, sample.unknown);
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
    const auto planes = [&] {
      return value::ConstPlanes{
          .value = {&sample.value, 1},
          .unknown = shapes_.operands[index]->IsFourState()
                         ? std::span<const std::uint64_t>{&sample.unknown, 1}
                         : std::span<const std::uint64_t>{}};
    };
    const auto bits = [&]() -> value::IntegralOperand {
      return value::BitsOperand{
          .planes = planes(), .width = shapes_.operands[index]->width};
    };
    const auto number = [&]() -> value::IntegralOperand {
      return value::NumberOperand{
          .planes = planes(),
          .width = shapes_.operands[index]->width,
          .signedness = shapes_.operands[index]->signedness};
    };
    switch (kinds[index]) {
      case IntegralOperandKind::kBits:
        return bits();
      case IntegralOperandKind::kNumber:
        return number();
      case IntegralOperandKind::kSameType:
        return support::IsNumberOperand(kinds.front()) ? number() : bits();
      case IntegralOperandKind::kMachineInt:
        return static_cast<std::int64_t>(sample.value);
      case IntegralOperandKind::kMachineBool:
        return sample.value != 0;
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
      llvm::IntegerType* const exact = builder.getIntNTy(
          static_cast<unsigned>(shapes_.operands[index]->width));
      return OneWord{
          .value = llvm::ConstantInt::get(exact, chosen_[index].value),
          .unknown = llvm::ConstantInt::get(exact, chosen_[index].unknown)};
    };
    const auto machine = [&](std::size_t index) -> llvm::Value* {
      switch (operation_->operands.Kinds()[index]) {
        case IntegralOperandKind::kMachineBool:
          return builder.getInt1(chosen_[index].value != 0);
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
      return builder.getInt64(chosen_[index].value);
    };
    const std::optional<OneWordAnswer> built = LowerOnOneWord(
        builder, operation_->op, shapes_,
        OneWordOperands{.planes = planes, .machine = machine});
    if (!built.has_value()) {
      has_arm_ = false;
      return;
    }

    std::vector<value::IntegralOperand> operands;
    operands.reserve(chosen_.size());
    for (std::size_t index = 0; index < chosen_.size(); ++index) {
      operands.push_back(LibraryOperand(index));
    }
    Answer stated;
    const bool answers_unknown =
        shapes_.answer.has_value() && shapes_.answer->IsFourState();
    const value::MachineAnswer stated_machine = value::ApplyIntegralOperation(
        operation_->op, operands,
        value::Planes{
            .value = {&stated.value, 1},
            .unknown = answers_unknown
                           ? std::span<std::uint64_t>{&stated.unknown, 1}
                           : std::span<std::uint64_t>{}},
        shapes_.answer.has_value() ? shapes_.answer->width : 0);

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
            [&](const OneWord& answer) {
              const std::optional<std::uint64_t> value = WordOf(answer.value);
              const std::optional<std::uint64_t> unknown =
                  WordOf(answer.unknown);
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
                  .value = *value, .unknown = answers_unknown ? *unknown : 0};
              if (inlined != stated) {
                disagrees(
                    std::format(
                        "answers value {:#x} unknown {:#x} on one word where "
                        "the library answers value {:#x} unknown {:#x}",
                        inlined.value, inlined.unknown, stated.value,
                        stated.unknown));
              }
            },
            [&](llvm::Value* answer) {
              const std::optional<std::uint64_t> word = WordOf(answer);
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
                        "answers the machine value {:#x} on one word where "
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
              if (fixed.width <= 64) {
                answering(
                    IntegralShape{
                        .width = fixed.width,
                        .signedness = Signedness::kUnsigned,
                        .domain = fixed.domain});
              }
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

// The two statements of an operation on one word -- the instructions the code
// generator builds for it and the library's function over planes -- answer
// alike, bit for bit in both planes, for every operation the code generator
// builds instructions for.
TEST(IntegralOneWordTest, EveryInlineOperationAnswersAsTheLibraryStatesIt) {
  std::set<std::string_view> without_arm;
  std::size_t compared = 0;
  for (const IntegralOperation& operation : support::IntegralOperations()) {
    Folding folding;
    if (!std::ranges::all_of(operation.operands.Kinds(), CanMake)) {
      // No operand is read before the operation is found to have no arm.
      const auto unread = [](std::size_t) -> OneWord {
        ADD_FAILURE() << "an operand this test cannot make was read";
        return OneWord{.value = nullptr, .unknown = nullptr};
      };
      const auto unread_machine = [](std::size_t) -> llvm::Value* {
        ADD_FAILURE() << "an operand this test cannot make was read";
        return nullptr;
      };
      EXPECT_FALSE(
          LowerOnOneWord(
              folding.builder, operation.op,
              IntegralShapes{
                  .operands = {std::nullopt},
                  .answer =
                      IntegralShape{
                          .width = 8,
                          .signedness = Signedness::kUnsigned,
                          .domain = StateDomain::kFourState}},
              OneWordOperands{.planes = unread, .machine = unread_machine})
              .has_value())
          << operation.name;
      without_arm.insert(operation.name);
      continue;
    }
    std::size_t compared_here = 0;
    bool has_arm = true;
    for (IntegralShapes& shapes : ApplicationsOf(operation)) {
      const std::optional<std::size_t> count =
          Application(folding, operation, std::move(shapes)).Compare();
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
          << " is built on one word for some types no wider and not others";
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
