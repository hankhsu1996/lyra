#include "lyra/runtime/member_storage.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
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
  switch (descriptor.kind) {
    case MemberStorageKind::kBorrowedHandle:
      object_.emplace<BorrowedHandle>();
      return;
    case MemberStorageKind::kCancellationSource:
      object_.emplace<CancellationSource>();
      return;
    case MemberStorageKind::kChannelCancellation:
      object_.emplace<ChannelCancellation>();
      return;
    case MemberStorageKind::kObservableCell:
      switch (descriptor.domain) {
        case ValueDomain::kPacked:
          object_.emplace<Var<value::PackedArray>>();
          return;
        case ValueDomain::kString:
          object_.emplace<Var<value::String>>();
          return;
        case ValueDomain::kReal:
          object_.emplace<Var<value::Real>>();
          return;
        case ValueDomain::kShortReal:
          object_.emplace<Var<value::ShortReal>>();
          return;
        case ValueDomain::kTuple:
          object_.emplace<Var<value::RuntimeTuple>>();
          return;
        case ValueDomain::kDynArray:
          object_.emplace<Var<value::RuntimeDynamicArray>>();
          return;
        case ValueDomain::kUnpackedArray:
          object_.emplace<Var<value::RuntimeUnpackedArray>>();
          return;
        case ValueDomain::kChandle:
          throw InternalError(
              "MemberStorage: a chandle is not observable storage");
        case ValueDomain::kNone:
          throw InternalError(
              "MemberStorage: an observable cell needs a value domain");
      }
      throw InternalError("MemberStorage: unknown value domain");
    case MemberStorageKind::kInlineValue:
      switch (descriptor.domain) {
        case ValueDomain::kChandle:
          object_.emplace<value::Chandle>();
          return;
        case ValueDomain::kPacked:
          object_.emplace<value::PackedArray>();
          return;
        case ValueDomain::kString:
          object_.emplace<value::String>();
          return;
        case ValueDomain::kReal:
          object_.emplace<value::Real>();
          return;
        case ValueDomain::kShortReal:
          object_.emplace<value::ShortReal>();
          return;
        case ValueDomain::kTuple:
          object_.emplace<value::RuntimeTuple>();
          return;
        case ValueDomain::kDynArray:
          object_.emplace<value::RuntimeDynamicArray>();
          return;
        case ValueDomain::kUnpackedArray:
          object_.emplace<value::RuntimeUnpackedArray>();
          return;
        case ValueDomain::kNone:
          throw InternalError(
              "MemberStorage: an inline value needs a value domain");
      }
      throw InternalError("MemberStorage: unknown value domain");
  }
  throw InternalError("MemberStorage: unknown member storage kind");
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
