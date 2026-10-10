#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {
namespace {

constexpr std::string_view kAbiHeader = "include/lyra/runtime/runtime_abi.hpp";

// What a parameter of a prototype hands the entry: a value, or one of the
// things an entry is told of a type -- how wide an integral type is, whether
// it is signed, whether it is four-state, or the constant of a type the entry
// keeps and acts on.
enum class Hands : std::uint8_t {
  kValue,
  kWidth,
  kSignedness,
  kFourState,
  kTypeConstant,
};

struct Parameter {
  std::string name;
  Hands hands;
};

auto IsIdentifier(char c) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c == '_';
}

auto Trimmed(std::string_view text) -> std::string_view {
  constexpr std::string_view kSpace = " \t\n";
  const std::size_t first = text.find_first_not_of(kSpace);
  if (first == std::string_view::npos) {
    return {};
  }
  return text.substr(first, text.find_last_not_of(kSpace) - first + 1);
}

// A parameter as the header spells it: its type and its name.
struct Spelled {
  std::string_view type;
  std::string_view name;
};

auto SpelledOf(std::string_view text) -> Spelled {
  std::size_t name_at = text.size();
  while (name_at > 0 && IsIdentifier(text[name_at - 1])) {
    --name_at;
  }
  return Spelled{
      .type = Trimmed(text.substr(0, name_at)), .name = text.substr(name_at)};
}

auto IsFlag(const Spelled& p, std::string_view suffix) -> bool {
  return p.type == "bool" && p.name.ends_with(suffix);
}

auto IsFourStateFlag(const Spelled& p) -> bool {
  return IsFlag(p, "_is_four_state") || IsFlag(p, "_are_four_state");
}

// What each parameter of one prototype hands the entry. The header names what
// an entry is told of an operand after the operand: a width is `width` alone,
// `_width` after an earlier parameter's name, after `out` for the answer's or
// after `bits` for the type some bits were written at, or `_width` ahead of
// the flags told of the same operand.
auto ParametersOf(std::span<const Spelled> spelled) -> std::vector<Parameter> {
  constexpr std::string_view kWidth = "_width";
  const auto names_a_width = [&](std::size_t at) {
    const std::string_view name = spelled[at].name;
    if (name == "width") {
      return true;
    }
    if (!name.ends_with(kWidth)) {
      return false;
    }
    const std::string_view of = name.substr(0, name.size() - kWidth.size());
    const auto earlier = spelled.first(at);
    if (of == "out" || of == "bits" ||
        std::ranges::any_of(
            earlier, [&](const Spelled& p) { return p.name == of; })) {
      return true;
    }
    return at + 1 < spelled.size() && spelled[at + 1].name.starts_with(of) &&
           (IsFlag(spelled[at + 1], "_is_signed") ||
            IsFourStateFlag(spelled[at + 1]));
  };
  std::vector<Parameter> parameters;
  parameters.reserve(spelled.size());
  for (std::size_t at = 0; at < spelled.size(); ++at) {
    const Spelled& p = spelled[at];
    const bool pointer = p.type.contains('*');
    Hands hands = Hands::kValue;
    if (!pointer && p.type.contains("std::int64_t") && names_a_width(at)) {
      hands = Hands::kWidth;
    } else if (IsFlag(p, "_is_signed")) {
      hands = Hands::kSignedness;
    } else if (IsFourStateFlag(p)) {
      hands = Hands::kFourState;
    } else if (pointer && p.name.ends_with("_type")) {
      hands = Hands::kTypeConstant;
    }
    parameters.push_back(
        Parameter{.name = std::string(p.name), .hands = hands});
  }
  return parameters;
}

using Prototypes = std::map<std::string, std::vector<Parameter>>;

// Every entry the header declares, against its parameters in order. A
// declaration opens its line with `auto` or `void`, after saying that it does
// not return where it does not, which is what tells one from a mention of the
// entry in a comment.
auto PrototypesIn(std::string_view text) -> Prototypes {
  constexpr std::string_view kPrefix = "lyra_rt_";
  constexpr std::array<std::string_view, 3> kLeads{
      "auto ", "void ", "[[noreturn]] void "};
  Prototypes prototypes;
  for (std::size_t at = text.find(kPrefix); at != std::string_view::npos;
       at = text.find(kPrefix, at + 1)) {
    const std::size_t line = text.rfind('\n', at) + 1;
    const std::string_view lead = text.substr(line, at - line);
    if (!std::ranges::contains(kLeads, lead)) {
      continue;
    }
    std::size_t open = at;
    while (open < text.size() && IsIdentifier(text[open])) {
      ++open;
    }
    if (open == text.size() || text[open] != '(') {
      continue;
    }
    std::vector<Spelled> spelled;
    std::size_t depth = 1;
    std::size_t start = open + 1;
    for (std::size_t end = start; end < text.size() && depth > 0; ++end) {
      const char c = text[end];
      if (c == '(') {
        ++depth;
      } else if (c == ')') {
        --depth;
      }
      if ((c == ',' && depth == 1) || depth == 0) {
        const std::string_view one = Trimmed(text.substr(start, end - start));
        if (!one.empty()) {
          spelled.push_back(SpelledOf(one));
        }
        start = end + 1;
      }
    }
    prototypes.emplace(
        std::string(text.substr(at, open - at)), ParametersOf(spelled));
  }
  return prototypes;
}

// One realization of an entry, as its declaration states it.
struct Declared {
  support::OperandReadings operands;
  Told told;
  support::AnswerTold answer_told;
  // The callee names the member a union is built holding, which crosses ahead
  // of the operands a call states.
  bool names_a_member_first;
};

// Where `declared` and `parameters` disagree, or nothing where every operand
// is taken as it is read: each declared reading takes the parameters its mode
// crosses as, in order, and nothing after the stated operands tells the entry
// of a type but what the declaration says it is told of its answer.
auto Disagreement(
    const Declared& declared, std::span<const Parameter> parameters)
    -> std::optional<std::string> {
  std::size_t at = declared.names_a_member_first ? 1 : 0;
  const auto take = [&](Hands hands) -> bool {
    if (at >= parameters.size() || parameters[at].hands != hands) {
      return false;
    }
    ++at;
    return true;
  };
  if (std::holds_alternative<ToldHeldWidthAhead>(declared.told) &&
      !take(Hands::kWidth)) {
    return "is declared told how wide the values it holds are ahead of its "
           "operands and does not open with a width";
  }
  // The operand a realization is told something of crosses with it, or as it,
  // and every other operand as its reading says. Which type is told is no
  // part of a prototype's shape, so any type stands for it.
  const auto mode_of = [&](std::size_t operand) -> PassMode {
    const PassMode read =
        PassModeOf(declared.operands.At(operand), lir::TypeId{});
    const auto told_at = [&](std::size_t concerned, PassMode told) {
      return concerned == operand ? told : read;
    };
    return std::visit(
        Overloaded{
            [&](const ToldNothing&) { return read; },
            [&](const ToldHeldWidthAhead&) { return read; },
            [&](const ToldHeldWidth& t) {
              return told_at(t.after, PassWithWidth{.type = lir::TypeId{}});
            },
            [&](const ToldKeyType& t) {
              return told_at(t.after, PassWithType{.type = lir::TypeId{}});
            },
            [&](const ToldMemberType& t) {
              return told_at(t.after, PassWithType{.type = lir::TypeId{}});
            }},
        declared.told);
  };
  const std::size_t stated = std::max(
      declared.operands.All().size(),
      std::visit(
          Overloaded{
              [](const ToldNothing&) -> std::size_t { return 0; },
              [](const ToldHeldWidthAhead&) -> std::size_t { return 0; },
              [](const ToldHeldWidth& t) { return t.after + 1; },
              [](const ToldKeyType& t) { return t.after + 1; },
              [](const ToldMemberType& t) { return t.after + 1; }},
          declared.told));
  for (std::size_t i = 0; i < stated; ++i) {
    const std::size_t began = at;
    const bool taken = std::visit(
        Overloaded{
            [&](const PassDirect&) { return take(Hands::kValue); },
            [&](const PassWithExtent&) {
              return take(Hands::kValue) && take(Hands::kWidth) &&
                     take(Hands::kFourState);
            },
            [&](const PassWithShape&) {
              return take(Hands::kValue) && take(Hands::kWidth) &&
                     take(Hands::kSignedness) && take(Hands::kFourState);
            },
            [&](const PassWithType&) {
              return take(Hands::kValue) && take(Hands::kTypeConstant);
            },
            [&](const PassWithWidth&) {
              return take(Hands::kValue) && take(Hands::kWidth);
            }},
        mode_of(i));
    if (!taken) {
      return std::format(
          "does not take operand {} the way its declaration reads it: the "
          "parameters from position {} do not fit, at position {}",
          i, began, at);
    }
  }
  std::vector<Parameter> told;
  for (; at < parameters.size(); ++at) {
    switch (parameters[at].hands) {
      case Hands::kValue:
        break;
      case Hands::kWidth:
      case Hands::kSignedness:
      case Hands::kFourState:
      case Hands::kTypeConstant:
        told.push_back(parameters[at]);
        break;
    }
  }
  const auto told_names = [&] {
    std::string names;
    for (const Parameter& p : told) {
      names += std::format(" `{}`", p.name);
    }
    return names;
  };
  const auto hands = [&](std::size_t index, Hands what) {
    return index < told.size() && told[index].hands == what;
  };
  switch (declared.answer_told) {
    case support::AnswerTold::kNothing:
      if (!told.empty()) {
        return std::format(
            "takes{} past the operands its declaration states a reading of",
            told_names());
      }
      return std::nullopt;
    case support::AnswerTold::kElementType:
      if (told.size() != 1 || !hands(0, Hands::kTypeConstant)) {
        return std::format(
            "is declared told the element type of its answer and takes{}",
            told_names());
      }
      return std::nullopt;
    case support::AnswerTold::kIntegralExtent:
      if (told.size() != 2 || !hands(0, Hands::kWidth) ||
          !hands(1, Hands::kFourState)) {
        return std::format(
            "is declared told the extent of its answer and takes{}",
            told_names());
      }
      return std::nullopt;
    case support::AnswerTold::kIntegralWidth:
      if (told.size() != 1 || !hands(0, Hands::kWidth)) {
        return std::format(
            "is declared told how wide the type it is called at is and "
            "takes{}",
            told_names());
      }
      return std::nullopt;
  }
  throw InternalError("entry prototype test: unknown answer told");
}

// Every value of a closed set whose enumerators count up from zero, found by
// asking `name` for each one's spelling until it refuses a value.
template <typename Enum, typename Name>
auto Every(const Name& name) -> std::vector<Enum> {
  std::vector<Enum> all;
  for (std::uint32_t i = 0;; ++i) {
    const auto one = static_cast<Enum>(i);
    try {
      if (std::string_view(name(one)).empty()) {
        throw InternalError("entry prototype test: an enumerator has no name");
      }
    } catch (const InternalError&) {
      return all;
    }
    all.push_back(one);
  }
}

// Every realization the declarations state, by the symbol each is published
// under, and what was stated more than once or names no function.
struct Realizations {
  std::map<std::string, Declared> by_symbol;
  // A symbol two declarations compose.
  std::vector<std::string> stated_twice;
  // A symbol a declaration composes for an entry that is a function, so the
  // header has to declare it.
  std::vector<std::string> functions;
};

auto DeclaredRealizations() -> Realizations {
  const std::vector<support::BuiltinFn> builtins = Every<support::BuiltinFn>(
      [](support::BuiltinFn fn) { return support::RuntimeEntryOf(fn).name; });
  const std::vector<RuntimeOp> runtime_ops =
      Every<RuntimeOp>([](RuntimeOp op) { return RuntimeSymbol(op); });
  const std::vector<support::ValueDomain> domains =
      Every<support::ValueDomain>([](support::ValueDomain domain) {
        return support::ValueDomainName(domain);
      });

  Realizations all;
  const auto state = [&](const std::string& symbol, const Declared& declared) {
    if (!all.by_symbol.emplace(symbol, declared).second) {
      all.stated_twice.push_back(symbol);
    }
  };
  const auto builtin = [&](const std::string& symbol, support::BuiltinFn fn,
                           const Told& told) {
    const support::RuntimeEntry entry = support::RuntimeEntryOf(fn);
    state(
        symbol, Declared{
                    .operands = entry.operands,
                    .told = told,
                    .answer_told = entry.answer_told,
                    .names_a_member_first =
                        std::holds_alternative<ToldMemberType>(told)});
    all.functions.push_back(symbol);
  };
  const auto over_each = [&](std::span<const RealizedFor> realized,
                             const auto& stated) {
    for (const RealizedFor& one : realized) {
      for (const support::ValueDomain domain : domains) {
        if (one.domains.Holds(domain)) {
          stated(domain, one.told);
        }
      }
    }
  };

  for (const support::BuiltinFn fn : builtins) {
    const auto per_domain = [&](std::span<const RealizedFor> realized) {
      over_each(realized, [&](support::ValueDomain domain, const Told& told) {
        builtin(RuntimeSymbol(domain, fn), fn, told);
      });
    };
    std::visit(
        Overloaded{
            [&](const NamedAlone&) {
              builtin(RuntimeSymbol(fn), fn, ToldNothing{});
            },
            [](const OverIntegralValues&) {}, [](const NotRealized&) {},
            [&](const NamedByValue& named) { per_domain(named.realized); },
            [&](const NamedByResult& named) { per_domain(named.realized); },
            [&](const NamedByStorageDomain& named) {
              per_domain(named.realized);
            },
            [&](const NamedByWrapper& named) {
              for (const ThroughWrapper& one : named.realized) {
                for (const support::ValueDomain domain : domains) {
                  if (one.domains.Holds(domain)) {
                    builtin(
                        RuntimeSymbol(domain, one.wrapper, fn), fn, one.told);
                  }
                }
              }
            },
            [&](const NamedByConversion& named) {
              for (const Conversion& one : named.realized) {
                builtin(
                    RuntimeSymbol(one.destination, fn, one.source), fn,
                    ToldNothing{});
              }
            }},
        EntryNamingOf(fn));
  }
  for (const RuntimeOp op : runtime_ops) {
    const auto stated = [&](const std::string& symbol, const Told& told) {
      state(
          symbol, Declared{
                      .operands = OperandReadingsOf(op),
                      .told = told,
                      .answer_told = AnswerToldOf(op),
                      .names_a_member_first = false});
    };
    // An entry realized once, or per object, is published under its own name
    // where it is published at all, which nothing here states.
    stated(RuntimeSymbol(op), ToldNothing{});
    over_each(
        RealizationsOf(op), [&](support::ValueDomain domain, const Told& told) {
          stated(RuntimeSymbol(domain, op), told);
          all.functions.push_back(RuntimeSymbol(domain, op));
        });
  }
  const auto holder = [&](const auto& op, support::AnswerTold answer_told) {
    over_each(
        RealizationsOf(op), [&](support::ValueDomain domain, const Told& told) {
          state(
              RuntimeSymbol(domain, op), Declared{
                                             .operands = OperandReadingsOf(op),
                                             .told = told,
                                             .answer_told = answer_told,
                                             .names_a_member_first = false});
          all.functions.push_back(RuntimeSymbol(domain, op));
        });
  };
  for (const lir::ValueCellTarget::Op op :
       {lir::ValueCellTarget::Op::kAllocate, lir::ValueCellTarget::Op::kLoad,
        lir::ValueCellTarget::Op::kStore}) {
    holder(op, support::AnswerTold::kNothing);
  }
  for (const lir::OpenWriteTarget::Op op :
       {lir::OpenWriteTarget::Op::kLand, lir::OpenWriteTarget::Op::kAssignSlice,
        lir::OpenWriteTarget::Op::kReadSlice}) {
    holder(op, support::AnswerTold::kNothing);
  }
  for (const lir::DesignatedBitsTarget::Op op :
       {lir::DesignatedBitsTarget::Op::kRead,
        lir::DesignatedBitsTarget::Op::kPlace,
        lir::DesignatedBitsTarget::Op::kReport}) {
    holder(op, AnswerToldOf(op));
  }
  // The entry building a member's storage, for each kind of storage that holds
  // values over each domain the library lays that storage out for.
  for (const support::MemberStorageKind kind :
       {support::MemberStorageKind::kObservableCell,
        support::MemberStorageKind::kResolvedNet,
        support::MemberStorageKind::kSampledHistory,
        support::MemberStorageKind::kValueCell}) {
    for (const support::ValueDomain domain : domains) {
      const support::DeclaredMemberStorage storage{
          .kind = kind, .domain = domain};
      try {
        runtime::LayoutOf(storage);
      } catch (const InternalError&) {
        continue;
      }
      const std::string symbol = RuntimeSymbol(storage, RuntimeOp::kConstruct);
      state(
          symbol, Declared{
                      .operands = {},
                      .told = BuiltAtTheWidthHeld(storage)
                                  ? Told{ToldHeldWidth{.after = 0}}
                                  : Told{ToldNothing{}},
                      .answer_told = support::AnswerTold::kNothing,
                      .names_a_member_first = false});
      all.functions.push_back(symbol);
    }
  }
  return all;
}

auto AbiPrototypes() -> Prototypes {
  std::ifstream header{std::string(kAbiHeader)};
  const std::string text{
      std::istreambuf_iterator<char>(header), std::istreambuf_iterator<char>()};
  return PrototypesIn(text);
}

// Every entry the library publishes takes each operand the way its one
// declaration reads it, at the position the declaration reads it at, and takes
// nothing that tells it of a type the declaration does not state. A prototype
// no declaration composes the name of is told of no type at all.
TEST(EntryPrototypeTest, EveryPrototypeTakesWhatItsDeclarationReads) {
  const Prototypes prototypes = AbiPrototypes();
  ASSERT_GT(prototypes.size(), 1000U) << "the ABI header was not read";
  const Realizations declared = DeclaredRealizations();

  std::string disagreements;
  std::size_t held = 0;
  for (const auto& [symbol, parameters] : prototypes) {
    const auto found = declared.by_symbol.find(symbol);
    if (found == declared.by_symbol.end()) {
      const auto told = std::ranges::find_if(
          parameters,
          [](const Parameter& p) { return p.hands != Hands::kValue; });
      if (told != parameters.end()) {
        disagreements += std::format(
            "  {} takes `{}` and no declaration states how it reads its "
            "operands\n",
            symbol, told->name);
      }
      continue;
    }
    ++held;
    if (const std::optional<std::string> disagreement =
            Disagreement(found->second, parameters)) {
      disagreements += std::format("  {} {}\n", symbol, *disagreement);
    }
  }
  EXPECT_GT(held, 500U) << "few prototypes were matched to a declaration";
  EXPECT_TRUE(disagreements.empty()) << disagreements;
}

// A symbol names one realization of one entry, so no two declarations compose
// the same one.
TEST(EntryPrototypeTest, NoSymbolIsStatedTwice) {
  std::string twice;
  for (const std::string& symbol : DeclaredRealizations().stated_twice) {
    twice += std::format("  {}\n", symbol);
  }
  EXPECT_TRUE(twice.empty()) << twice;
}

// Every realization a declaration states is one the library publishes, so a
// call on any of them links.
TEST(EntryPrototypeTest, EveryStatedRealizationIsPublished) {
  const Prototypes prototypes = AbiPrototypes();
  std::string missing;
  for (const std::string& symbol : DeclaredRealizations().functions) {
    if (!prototypes.contains(symbol)) {
      missing += std::format("  {}\n", symbol);
    }
  }
  EXPECT_TRUE(missing.empty()) << missing;
}

// A realization the declarations do not state is refused where its symbol is
// composed.
TEST(EntryPrototypeTest, AnUnstatedRealizationIsRefused) {
  EXPECT_THROW(
      RuntimeSymbol(support::ValueDomain::kString, support::BuiltinFn::kSize),
      InternalError);
  EXPECT_THROW(
      RuntimeSymbol(
          support::ValueDomain::kString, WrapperKind::kNet,
          support::BuiltinFn::kLoad),
      InternalError);
  EXPECT_THROW(
      RuntimeSymbol(support::ValueDomain::kBit8, RuntimeOp::kCopy),
      InternalError);
  EXPECT_NO_THROW(
      RuntimeSymbol(support::ValueDomain::kQueue, support::BuiltinFn::kSize));
}

// The comparison refuses a declaration that leaves out a reading the prototype
// takes, one that states a reading at another position, and one that reads an
// operand as bits where the prototype takes a number.
TEST(EntryPrototypeTest, ADeclarationThatDisagreesIsRefused) {
  using enum support::OperandReading;
  const std::vector<Parameter> parameters =
      PrototypesIn(
          "void lyra_rt_probe(void* queue, const void* position, "
          "const void* item, std::int64_t item_width, bool item_is_signed, "
          "bool item_is_four_state);")
          .at("lyra_rt_probe");
  const auto declared = [](support::OperandReadings operands) {
    return Declared{
        .operands = operands,
        .told = ToldNothing{},
        .answer_told = support::AnswerTold::kNothing,
        .names_a_member_first = false};
  };
  EXPECT_FALSE(
      Disagreement(declared({kMachine, kPosition, kNumber}), parameters)
          .has_value());
  EXPECT_TRUE(
      Disagreement(declared({kMachine, kPosition}), parameters).has_value());
  EXPECT_TRUE(Disagreement(declared({kMachine, kNumber, kPosition}), parameters)
                  .has_value());
  EXPECT_TRUE(Disagreement(declared({kMachine, kPosition, kBits}), parameters)
                  .has_value());
}

}  // namespace
}  // namespace lyra::backend::llvm_backend
