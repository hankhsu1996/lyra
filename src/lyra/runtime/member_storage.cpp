#include "lyra/runtime/member_storage.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

template <typename T>
auto Read(const void* handle) -> const T& {
  return *static_cast<const T*>(handle);
}

}  // namespace

MemberStorage::MemberStorage(MemberStorageDescriptor descriptor) {
  std::visit(
      Overloaded{
          [this](const BorrowedHandleStorage&) {
            object_.emplace<BorrowedHandle>();
          },
          [this](const CancellationSourceStorage&) {
            object_.emplace<CancellationSource>();
          },
          [this](const ChannelCancellationStorage&) {
            object_.emplace<ChannelCancellation>();
          },
          [this](const ObservableCellStorage& cell) {
            switch (cell.domain) {
              case support::ValueDomain::kPacked:
                object_.emplace<Var<value::PackedArray>>();
                return;
              case support::ValueDomain::kString:
                object_.emplace<Var<value::String>>();
                return;
              case support::ValueDomain::kReal:
                object_.emplace<Var<value::Real>>();
                return;
              case support::ValueDomain::kShortReal:
                object_.emplace<Var<value::ShortReal>>();
                return;
              case support::ValueDomain::kTuple:
                object_.emplace<Var<value::RuntimeTuple>>();
                return;
              case support::ValueDomain::kDynArray:
                object_.emplace<Var<value::RuntimeDynamicArray>>();
                return;
              case support::ValueDomain::kUnpackedArray:
                object_.emplace<Var<value::RuntimeUnpackedArray>>();
                return;
              case support::ValueDomain::kQueue:
                object_.emplace<Var<value::RuntimeQueue>>();
                return;
              case support::ValueDomain::kAssocArray:
                object_.emplace<Var<value::RuntimeAssociativeArray>>();
                return;
              // A chandle is a value its owner holds, never a cell other
              // processes wait on (LRM 6.14): nothing subscribes to it.
              case support::ValueDomain::kChandle:
                throw InternalError(
                    "MemberStorage: a chandle is not observable storage");
            }
            throw InternalError("MemberStorage: unknown value domain");
          },
          [this](const InlineValueStorage& inline_value) {
            switch (inline_value.domain) {
              case support::ValueDomain::kChandle:
                object_.emplace<value::Chandle>();
                return;
              case support::ValueDomain::kPacked:
                object_.emplace<value::PackedArray>();
                return;
              case support::ValueDomain::kString:
                object_.emplace<value::String>();
                return;
              case support::ValueDomain::kReal:
                object_.emplace<value::Real>();
                return;
              case support::ValueDomain::kShortReal:
                object_.emplace<value::ShortReal>();
                return;
              case support::ValueDomain::kTuple:
                object_.emplace<value::RuntimeTuple>();
                return;
              case support::ValueDomain::kDynArray:
                object_.emplace<value::RuntimeDynamicArray>();
                return;
              case support::ValueDomain::kUnpackedArray:
                object_.emplace<value::RuntimeUnpackedArray>();
                return;
              case support::ValueDomain::kQueue:
                object_.emplace<value::RuntimeQueue>();
                return;
              case support::ValueDomain::kAssocArray:
                object_.emplace<value::RuntimeAssociativeArray>();
                return;
            }
            throw InternalError("MemberStorage: unknown value domain");
          }},
      descriptor);
}

auto MemberStorage::Address() -> void* {
  return std::visit([](auto& cell) -> void* { return &cell; }, object_);
}

auto MemberStorage::HeldValue() -> void* {
  return std::visit(
      Overloaded{
          [](BorrowedHandle& box) -> void* { return box.target; },
          [](value::Chandle& chandle) -> void* { return chandle.Ptr(); },
          [](auto& object) -> void* { return &object; }},
      object_);
}

void MemberStorage::AdoptFrom(void* handle) {
  std::visit(
      Overloaded{
          // A pointer-shaped value is the handle, so there is nothing behind it
          // to read out.
          [&](BorrowedHandle& box) { box.target = handle; },
          [&](value::Chandle& chandle) { chandle = value::Chandle{handle}; },
          [](CancellationSource&) {
            throw InternalError(
                "MemberStorage: a cancellation source is created by the scope "
                "that owns it, never copied into storage");
          },
          []<typename T>(Var<T>&) {
            throw InternalError(
                "MemberStorage: a cell is written through its own access, "
                "where the write is an update event");
          },
          [&]<typename T>(T& value) { value = Read<T>(handle); }},
      object_);
}

}  // namespace lyra::runtime
