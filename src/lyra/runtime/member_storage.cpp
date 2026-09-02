#include "lyra/runtime/member_storage.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/net_resolution.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

template <typename T>
auto Read(const void* handle) -> const T& {
  return *static_cast<const T*>(handle);
}

// Realizes a net's resolution node over the value domain the net is declared
// in. LRM 6.7.1 admits a 4-state integral net and a fixed-size unpacked array,
// struct, or union of net-valid elements, and nothing else; a domain outside
// that set is one the front end should have rejected as a net's data type.
template <typename Resolver, typename Object>
void EmplaceResolvedNet(Object& object, support::ValueDomain domain) {
  switch (domain) {
    case support::ValueDomain::kPacked:
      object.template emplace<ResolvedNet<value::PackedArray, Resolver>>();
      return;
    case support::ValueDomain::kTuple:
      object.template emplace<ResolvedNet<value::RuntimeTuple, Resolver>>();
      return;
    case support::ValueDomain::kUnion:
      object.template emplace<ResolvedNet<value::RuntimeUnion, Resolver>>();
      return;
    case support::ValueDomain::kUnpackedArray:
      object.template emplace<
          ResolvedNet<value::RuntimeUnpackedArray, Resolver>>();
      return;
    case support::ValueDomain::kString:
    case support::ValueDomain::kReal:
    case support::ValueDomain::kShortReal:
    case support::ValueDomain::kChandle:
    case support::ValueDomain::kEmpty:
    case support::ValueDomain::kTaggedUnion:
    case support::ValueDomain::kDynArray:
    case support::ValueDomain::kQueue:
    case support::ValueDomain::kAssocArray:
    case support::ValueDomain::kManagedRef:
      throw InternalError(
          "MemberStorage: this value domain is not valid for a net (LRM "
          "6.7.1)");
  }
  throw InternalError("MemberStorage: unknown value domain");
}

}  // namespace

MemberStorage::MemberStorage(MemberStorageDescriptor descriptor) {
  std::visit(
      Overloaded{
          [this](const BorrowedHandleStorage&) {
            object_.emplace<BorrowedHandle>();
          },
          [this](const CancellationTargetStorage&) {
            object_.emplace<CancellationTarget>();
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
              case support::ValueDomain::kUnion:
                object_.emplace<Var<value::RuntimeUnion>>();
                return;
              case support::ValueDomain::kTaggedUnion:
                object_.emplace<Var<value::RuntimeTaggedUnion>>();
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
              // A class handle is likewise the value its owner holds (LRM 8.3);
              // what a process waits on is a property of the object it names,
              // never the handle.
              case support::ValueDomain::kManagedRef:
                throw InternalError(
                    "MemberStorage: a class handle is not observable storage");
              // An empty (void) value is only ever a tagged union's payload
              // (LRM 7.3.2), held inside its union, never storage of its own
              // that a process could wait on.
              case support::ValueDomain::kEmpty:
                throw InternalError(
                    "MemberStorage: an empty value is not observable storage");
            }
            throw InternalError("MemberStorage: unknown value domain");
          },
          [this](const ValueCellStorage& cell) {
            switch (cell.domain) {
              case support::ValueDomain::kPacked:
                object_.emplace<ActivationValueCell<value::PackedArray>>();
                return;
              case support::ValueDomain::kString:
                object_.emplace<ActivationValueCell<value::String>>();
                return;
              case support::ValueDomain::kReal:
                object_.emplace<ActivationValueCell<value::Real>>();
                return;
              case support::ValueDomain::kShortReal:
                object_.emplace<ActivationValueCell<value::ShortReal>>();
                return;
              case support::ValueDomain::kTuple:
                object_.emplace<ActivationValueCell<value::RuntimeTuple>>();
                return;
              case support::ValueDomain::kUnion:
                object_.emplace<ActivationValueCell<value::RuntimeUnion>>();
                return;
              case support::ValueDomain::kTaggedUnion:
                object_
                    .emplace<ActivationValueCell<value::RuntimeTaggedUnion>>();
                return;
              case support::ValueDomain::kDynArray:
                object_
                    .emplace<ActivationValueCell<value::RuntimeDynamicArray>>();
                return;
              case support::ValueDomain::kUnpackedArray:
                object_.emplace<
                    ActivationValueCell<value::RuntimeUnpackedArray>>();
                return;
              case support::ValueDomain::kQueue:
                object_.emplace<ActivationValueCell<value::RuntimeQueue>>();
                return;
              case support::ValueDomain::kAssocArray:
                object_.emplace<
                    ActivationValueCell<value::RuntimeAssociativeArray>>();
                return;
              // A pointer-shaped value is the pointer it carries (LRM 6.14,
              // 8.3), so a declaration gives it no representation for a write
              // to land at and a cell would have nothing to install.
              case support::ValueDomain::kChandle:
              case support::ValueDomain::kManagedRef:
                throw InternalError(
                    "MemberStorage: a pointer-shaped value has no cell");
              // An empty (void) value is a tagged union's payload (LRM 7.3.2),
              // held inside its union rather than in a cell of its own.
              case support::ValueDomain::kEmpty:
                throw InternalError(
                    "MemberStorage: an empty value has no cell of its own");
            }
            throw InternalError("MemberStorage: unknown value domain");
          },
          [this](const ResolvedNetStorage& net) {
            switch (net.resolution) {
              case support::NetResolution::kTriState:
                EmplaceResolvedNet<WireResolver>(object_, net.domain);
                return;
            }
            throw InternalError("MemberStorage: unknown net resolution");
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
              case support::ValueDomain::kUnion:
                object_.emplace<value::RuntimeUnion>();
                return;
              case support::ValueDomain::kTaggedUnion:
                object_.emplace<value::RuntimeTaggedUnion>();
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
              case support::ValueDomain::kManagedRef:
                object_.emplace<GcRef<ManagedObject>>();
                return;
              // An empty (void) value is a tagged union's payload (LRM 7.3.2),
              // held inside its union, never a member's own inline storage.
              case support::ValueDomain::kEmpty:
                throw InternalError(
                    "MemberStorage: an empty value is not a member's own "
                    "storage");
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
          // Storage a write can reach again hands nothing back in place: a
          // reader given the storage itself would see a later write change what
          // it already read, so reading copies out instead.
          []<typename T>(Var<T>&) -> void* {
            throw InternalError(
                "MemberStorage: a cell's contents are read through its own "
                "access, never handed back in place");
          },
          []<typename T>(ActivationValueCell<T>&) -> void* {
            throw InternalError(
                "MemberStorage: a variable's contents are read through its own "
                "access, never handed back in place");
          },
          // A net's value is the fold of its drivers' contributions, recomputed
          // as they change, so it is read through the node's own access for the
          // same reason a cell's contents are.
          []<typename T, typename R>(ResolvedNet<T, R>&) -> void* {
            throw InternalError(
                "MemberStorage: a net's resolved value is read through its own "
                "access, never handed back in place");
          },
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
          [](CancellationTarget&) {
            throw InternalError(
                "MemberStorage: a cancellation source is created by the scope "
                "that owns it, never copied into storage");
          },
          []<typename T>(Var<T>&) {
            throw InternalError(
                "MemberStorage: a cell is written through its own access, "
                "where the write is an update event");
          },
          []<typename T>(ActivationValueCell<T>&) {
            throw InternalError(
                "MemberStorage: a variable is written through its own store, "
                "which is what lands the write at the representation the "
                "declaration gave it");
          },
          // A net is never written, only driven (LRM 6.5): what reaches it is
          // a driver updating its own contribution, after which the net
          // re-resolves.
          []<typename T, typename R>(ResolvedNet<T, R>&) {
            throw InternalError(
                "MemberStorage: a net takes no store; a value reaches it only "
                "through one of its drivers");
          },
          [&]<typename T>(T& value) { value = Read<T>(handle); }},
      object_);
}

}  // namespace lyra::runtime
