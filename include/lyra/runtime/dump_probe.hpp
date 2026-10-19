#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>

#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/real.hpp"

// How each storage the library realizes is described to a value dump (LRM
// 21.7.1.2): a probe reading what it holds and naming where a change to it is
// reported, made where an instance describes itself. Generated code describes
// a member by naming it here, beside the constant stating its declaration.
namespace lyra::runtime {

namespace dump_detail {

// The words of an integral value laid out as its planes, `width` bits each,
// the unknown plane following the value plane where the value holds x or z.
inline auto WordsAt(const void* bytes, std::uint32_t width, bool four_state)
    -> value::ConstIntegralView {
  const std::size_t words = (std::size_t{width} + 63) / 64;
  const std::span<const std::uint64_t> all(
      static_cast<const std::uint64_t*>(bytes), four_state ? 2 * words : words);
  return {
      .value = all.first(words),
      .unknown = four_state ? all.subspan(words) : all.first(0),
      .width = width};
}

// Records `value` under `code`, at the width its declaration states: a layout
// holding values of several widths lays every one out at the widest, every
// position above the declared width clear.
template <class T>
void ReadValue(
    DumpCapture& into, DumpCode code, const T& value, std::uint32_t width) {
  if constexpr (requires { value.AsWords(); }) {
    const auto words = value.AsWords();
    value::ConstIntegralView view = words.View();
    view.width = width;
    into.Bits(code, view);
  } else {
    into.Real(code, static_cast<double>(value.Value()));
  }
}

// A variable whose value its type lays out where it lies.
template <value::LyraValue T>
class VarProbe final : public DumpProbe {
 public:
  VarProbe(Var<T>& cell, std::uint32_t width);
  VarProbe(const VarProbe&) = delete;
  auto operator=(const VarProbe&) -> VarProbe& = delete;
  VarProbe(VarProbe&&) = delete;
  auto operator=(VarProbe&&) -> VarProbe& = delete;
  ~VarProbe() override;

  void Read(DumpCapture& into, DumpCode code) const override {
    ReadValue(into, code, cell_->Get(), width_);
  }
  [[nodiscard]] auto Watched() const -> Trigger override {
    Trigger watched;
    watched.observable = cell_;
    return watched;
  }
  [[nodiscard]] auto Identity() const -> const void* override {
    return &cell_->Get();
  }

  [[nodiscard]] auto Member() const -> const void* override {
    return cell_;
  }

 private:
  Var<T>* cell_;
  std::uint32_t width_;
};

template <value::LyraValue T>
VarProbe<T>::VarProbe(Var<T>& cell, std::uint32_t width)
    : cell_(&cell), width_(width) {
}

template <value::LyraValue T>
VarProbe<T>::~VarProbe() = default;

// A variable whose words lie after it, as wide as its declaration states.
template <class HeldAs>
class TailCellProbe final : public DumpProbe {
 public:
  TailCellProbe(TailCell<HeldAs>& cell, std::uint32_t width, bool four_state)
      : cell_(&cell), width_(width), four_state_(four_state) {
  }
  void Read(DumpCapture& into, DumpCode code) const override {
    into.Bits(code, WordsAt(cell_->Bytes(), width_, four_state_));
  }
  [[nodiscard]] auto Watched() const -> Trigger override {
    Trigger watched;
    watched.observable = cell_;
    return watched;
  }
  [[nodiscard]] auto Identity() const -> const void* override {
    return cell_->Bytes();
  }

  [[nodiscard]] auto Member() const -> const void* override {
    return cell_;
  }

 private:
  TailCell<HeldAs>* cell_;
  std::uint32_t width_;
  bool four_state_;
};

// What a net shows, which lies in the net or in the object its one driver
// carries, and which a change is reported for through the member naming it.
template <class Net, class T>
class NetProbe final : public DumpProbe {
 public:
  NetProbe(Net& net, std::uint32_t width) : net_(&net), width_(width) {
  }
  void Read(DumpCapture& into, DumpCode code) const override {
    if constexpr (value::HeldAsWords<T>) {
      into.Bits(code, WordsAt(net_->Shown(), width_, true));
    } else {
      ReadValue(into, code, *static_cast<const T*>(net_->Shown()), width_);
    }
  }
  // Enrolled through the member naming what the net shows, as a wait on the
  // net is, so the watch follows the net to the object its one driver carries
  // and back.
  [[nodiscard]] auto Watched() const -> Trigger override {
    return Trigger(
        WatchedPlace::Through(net_->Member()), Observation{}, {}, 0, 0);
  }
  [[nodiscard]] auto Identity() const -> const void* override {
    return net_;
  }

  [[nodiscard]] auto Member() const -> const void* override {
    return net_;
  }

 private:
  Net* net_;
  std::uint32_t width_;
};

// A port standing for what its connection holds (LRM 23.3.3), read where the
// reference it was bound to names, and a second name for that thing where it
// names the whole of it (LRM 21.7.2.3 `$var` b).
template <class T>
class ReferenceProbe final : public DumpProbe {
 public:
  ReferenceProbe(const ErasedReference& reference, std::uint32_t width);
  ReferenceProbe(const ReferenceProbe&) = delete;
  auto operator=(const ReferenceProbe&) -> ReferenceProbe& = delete;
  ReferenceProbe(ReferenceProbe&&) = delete;
  auto operator=(ReferenceProbe&&) -> ReferenceProbe& = delete;
  ~ReferenceProbe() override;

  void Read(DumpCapture& into, DumpCode code) const override {
    if constexpr (value::HeldAsWords<T>) {
      into.Bits(
          code, WordsAt(
                    reference_->storage, width_,
                    T::kDomain == value::StateDomain::kFourState));
    } else {
      ReadValue(
          into, code, *static_cast<const T*>(reference_->storage), width_);
    }
  }
  // Enrolled through the port's member, as a wait on the port is, so the
  // watch follows a force that binds the member elsewhere (LRM 10.6.2).
  [[nodiscard]] auto Watched() const -> Trigger override {
    return Trigger(WatchedPlace::Through(*reference_), Observation{}, {}, 0, 0);
  }
  [[nodiscard]] auto Identity() const -> const void* override {
    if (const HeldByNet* net = std::get_if<HeldByNet>(&reference_->holder)) {
      return net->net;
    }
    return reference_->storage;
  }

  [[nodiscard]] auto Member() const -> const void* override {
    return reference_;
  }

 private:
  const ErasedReference* reference_;
  std::uint32_t width_;
};

template <class T>
ReferenceProbe<T>::ReferenceProbe(
    const ErasedReference& reference, std::uint32_t width)
    : reference_(&reference), width_(width) {
}

template <class T>
ReferenceProbe<T>::~ReferenceProbe() = default;

}  // namespace dump_detail

// The describer the running dump installed, which an instance describes
// itself to.
auto Describing(RuntimeEffects& runtime) -> Describer&;

template <value::LyraValue T>
void DescribeTo(Var<T>& cell, RuntimeEffects& runtime, const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry, std::make_unique<dump_detail::VarProbe<T>>(cell, entry->width));
}

template <value::StateDomain kDomain>
void DescribeTo(
    TailCell<WordsStorage<kDomain>>& cell, RuntimeEffects& runtime,
    const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry,
      std::make_unique<dump_detail::TailCellProbe<WordsStorage<kDomain>>>(
          cell, entry->width, kDomain == value::StateDomain::kFourState));
}

template <NetValue T>
void DescribeTo(
    ResolvedNet<T>& net, RuntimeEffects& runtime, const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry, std::make_unique<dump_detail::NetProbe<ResolvedNet<T>, T>>(
                  net, entry->width));
}

// A port as the reference it is bound to, typed by the value it names.
template <class T>
void DescribeReference(
    const ErasedReference& port, RuntimeEffects& runtime,
    const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry,
      std::make_unique<dump_detail::ReferenceProbe<T>>(port, entry->width));
}

template <value::LyraValue T>
void DescribeTo(
    VariableRef<T>& port, RuntimeEffects& runtime, const DumpEntry* entry) {
  DescribeReference<T>(port.Erased(), runtime, entry);
}

// A named event, recorded in a time step it was triggered in (LRM 21.7.2.2).
void DescribeTo(
    NamedEvent& event, RuntimeEffects& runtime, const DumpEntry* entry);

// A named block, task or function the instance declares, around the things
// its static variables are (LRM 21.7.2.3 `$scope`).
void BeginDescribedBlock(RuntimeEffects& runtime, const DumpBlock* block);
void EndDescribedBlock(RuntimeEffects& runtime);

// A real's value type is fixed by the language rather than shaped by the
// design, so what describes one is compiled once, here, rather than in every
// unit that declares one.
extern template class dump_detail::VarProbe<value::Real>;
extern template class dump_detail::VarProbe<value::ShortReal>;
extern template class dump_detail::ReferenceProbe<value::Real>;
extern template class dump_detail::ReferenceProbe<value::ShortReal>;
extern template void DescribeTo(
    Var<value::Real>& cell, RuntimeEffects& runtime, const DumpEntry* entry);
extern template void DescribeTo(
    Var<value::ShortReal>& cell, RuntimeEffects& runtime,
    const DumpEntry* entry);
extern template void DescribeTo(
    VariableRef<value::Real>& port, RuntimeEffects& runtime,
    const DumpEntry* entry);
extern template void DescribeTo(
    VariableRef<value::ShortReal>& port, RuntimeEffects& runtime,
    const DumpEntry* entry);

}  // namespace lyra::runtime
