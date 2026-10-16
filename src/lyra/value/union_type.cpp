#include "lyra/value/union_type.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/kept_copy.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

[[noreturn]] void NeverAskedOfAUnion() {
  throw InternalError(
      "UnionType: an operation is asked of a union that the language defines "
      "for no union -- please report this as a bug");
}

[[noreturn]] void NeverAskedOfATaggedUnion() {
  throw InternalError(
      "TaggedUnionType: an operation is asked of a tagged union that the "
      "language defines for no tagged union -- please report this as a bug");
}

// The value of `member` the bits at `value` stand for, read as its type holds
// one (LRM 7.3.2), kept aside while the operation reads it.
class OverlaidRead {
 public:
  OverlaidRead(const ValueType& member, const void* value)
      : kept_(member, value) {
    member.ClearAboveWidths(kept_.Bytes());
  }

  [[nodiscard]] auto Bytes() const -> const void* {
    return kept_.Bytes();
  }

 private:
  KeptCopy kept_;
};

}  // namespace

UnionType::UnionType(
    std::uint32_t size, std::uint32_t align,
    std::span<const ValueType* const> members)
    : ValueType(size, align),
      count_(static_cast<std::uint32_t>(members.size())),
      members_(members.data()) {
}

auto UnionType::CountStatedAt() const -> StatedAt {
  return StatedAtIn(this, count_);
}

auto UnionType::MembersStatedAt() const -> StatedAt {
  return StatedAtIn(this, members_);
}

auto UnionType::First() const -> const ValueType& {
  return *Members().front();
}

void UnionType::Copy(const void* value, void* out) const {
  std::memcpy(out, value, Size());
}

void UnionType::Move(void* value, void* out) const noexcept {
  std::memcpy(out, value, Size());
}

void UnionType::Destroy(void*) const noexcept {
}

void UnionType::Assign(void* storage, const void* value) const {
  std::memmove(storage, value, Size());
}

auto UnionType::Equal(const void* lhs, const void* rhs) const -> FourStateBit {
  return First().Equal(
      OverlaidRead(First(), lhs).Bytes(), OverlaidRead(First(), rhs).Bytes());
}

auto UnionType::CaseEqual(const void* lhs, const void* rhs) const -> bool {
  return First().CaseEqual(
      OverlaidRead(First(), lhs).Bytes(), OverlaidRead(First(), rhs).Bytes());
}

auto UnionType::BitIdentical(const void* lhs, const void* rhs) const -> bool {
  return std::memcmp(lhs, rhs, Size()) == 0;
}

auto UnionType::HasUnknown(const void* value) const -> bool {
  return First().HasUnknown(OverlaidRead(First(), value).Bytes());
}

// A union's bits are whatever its members' are, so it keeps none of them
// clear of its own.
void UnionType::ClearAboveWidths(void*) const {
}

void UnionType::BuildDefault(void* out) const {
  std::memset(out, 0, Size());
  First().BuildDefault(out);
}

void UnionType::BitstreamWidth(const void* value, void* out) const {
  First().BitstreamWidth(OverlaidRead(First(), value).Bytes(), out);
}

void UnionType::CountBits(
    const void* value, const ConstIntegralView& control_bits, void* out) const {
  First().CountBits(OverlaidRead(First(), value).Bytes(), control_bits, out);
}

auto UnionType::WriteToStream(
    const void* value, const IntegralView& stream, std::uint64_t filled) const
    -> std::uint64_t {
  return First().WriteToStream(
      OverlaidRead(First(), value).Bytes(), stream, filled);
}

auto UnionType::ReadFromStream(
    const ConstIntegralView& stream, std::uint64_t taken, void* out) const
    -> std::uint64_t {
  std::memset(out, 0, Size());
  return First().ReadFromStream(stream, taken, out);
}

void UnionType::ResolveTriState(
    const void* lhs, const void* rhs, void* out) const {
  std::memset(out, 0, Size());
  First().ResolveTriState(
      OverlaidRead(First(), lhs).Bytes(), OverlaidRead(First(), rhs).Bytes(),
      out);
}

void UnionType::ResolveWiredAnd(
    const void* lhs, const void* rhs, void* out) const {
  std::memset(out, 0, Size());
  First().ResolveWiredAnd(
      OverlaidRead(First(), lhs).Bytes(), OverlaidRead(First(), rhs).Bytes(),
      out);
}

void UnionType::ResolveWiredOr(
    const void* lhs, const void* rhs, void* out) const {
  std::memset(out, 0, Size());
  First().ResolveWiredOr(
      OverlaidRead(First(), lhs).Bytes(), OverlaidRead(First(), rhs).Bytes(),
      out);
}

void UnionType::Dominating(
    const void* stronger, const void* weaker, void* out) const {
  std::memset(out, 0, Size());
  First().Dominating(
      OverlaidRead(First(), stronger).Bytes(),
      OverlaidRead(First(), weaker).Bytes(), out);
}

void UnionType::FilledLike(
    const void* prototype, const void* fill, void* out) const {
  std::memset(out, 0, Size());
  First().FilledLike(OverlaidRead(First(), prototype).Bytes(), fill, out);
}

auto UnionType::OrderBefore(const void*, const void*) const -> bool {
  NeverAskedOfAUnion();
}

auto UnionType::IsTrue(const void*) const -> bool {
  NeverAskedOfAUnion();
}

void UnionType::Reduce(Reduction, const void*, const void*, void*) const {
  NeverAskedOfAUnion();
}

auto UnionType::Parts() const -> const PartsByPosition* {
  return nullptr;
}

auto UnionType::AsIntegral() const -> const IntegralValueType* {
  return nullptr;
}

auto UnionType::AsFixedArray() const -> const FixedArrayType* {
  return nullptr;
}

TaggedUnionType::TaggedUnionType(
    std::uint32_t size, std::uint32_t align,
    std::span<const ValueType* const> members, std::uint32_t tag_offset,
    std::uint32_t tag_bytes)
    : ValueType(size, align),
      count_(static_cast<std::uint32_t>(members.size())),
      members_(members.data()),
      tag_offset_(tag_offset),
      tag_bytes_(tag_bytes) {
}

auto TaggedUnionType::CountStatedAt() const -> StatedAt {
  return StatedAtIn(this, count_);
}

auto TaggedUnionType::MembersStatedAt() const -> StatedAt {
  return StatedAtIn(this, members_);
}

auto TaggedUnionType::TagOffsetStatedAt() const -> StatedAt {
  return StatedAtIn(this, tag_offset_);
}

auto TaggedUnionType::TagBytesStatedAt() const -> StatedAt {
  return StatedAtIn(this, tag_bytes_);
}

auto TaggedUnionType::Tag(const void* value) const -> std::uint64_t {
  static_assert(
      std::endian::native == std::endian::little,
      "a tag narrower than a word is the low bytes of one");
  std::uint64_t tag = 0;
  std::memcpy(&tag, Offset(value, tag_offset_), tag_bytes_);
  return tag;
}

void TaggedUnionType::SetTag(void* value, std::uint64_t tag) const {
  std::memcpy(Offset(value, tag_offset_), &tag, tag_bytes_);
}

auto TaggedUnionType::Held(const void* value) const -> const ValueType* {
  const std::uint64_t tag = Tag(value);
  return tag < count_ ? Members()[tag] : nullptr;
}

void TaggedUnionType::Copy(const void* value, void* out) const {
  std::memcpy(out, value, Size());
  if (const ValueType* const held = Held(value)) {
    held->Copy(value, out);
  }
}

void TaggedUnionType::Move(void* value, void* out) const noexcept {
  std::memcpy(out, value, Size());
  if (const ValueType* const held = Held(value)) {
    held->Move(value, out);
  }
}

void TaggedUnionType::Destroy(void* value) const noexcept {
  if (const ValueType* const held = Held(value)) {
    held->Destroy(value);
  }
}

void TaggedUnionType::Assign(void* storage, const void* value) const {
  if (storage == value) {
    return;
  }
  Destroy(storage);
  Copy(value, storage);
}

auto TaggedUnionType::Equal(const void* lhs, const void* rhs) const
    -> FourStateBit {
  const ValueType* const held = Held(lhs);
  if (held == nullptr || Tag(lhs) != Tag(rhs)) {
    return FourStateBit::kZero;
  }
  return held->Equal(lhs, rhs);
}

auto TaggedUnionType::CaseEqual(const void* lhs, const void* rhs) const
    -> bool {
  const ValueType* const held = Held(lhs);
  return held != nullptr && Tag(lhs) == Tag(rhs) && held->CaseEqual(lhs, rhs);
}

auto TaggedUnionType::BitIdentical(const void* lhs, const void* rhs) const
    -> bool {
  const ValueType* const held = Held(lhs);
  if (held == nullptr) {
    return std::memcmp(lhs, rhs, Size()) == 0;
  }
  return Tag(lhs) == Tag(rhs) && held->BitIdentical(lhs, rhs);
}

auto TaggedUnionType::HasUnknown(const void* value) const -> bool {
  const ValueType* const held = Held(value);
  return held != nullptr && held->HasUnknown(value);
}

void TaggedUnionType::ClearAboveWidths(void* value) const {
  if (const ValueType* const held = Held(value)) {
    held->ClearAboveWidths(value);
  }
}

// LRM 11.9 leaves a tagged union no one has written undefined, tag included;
// it holds its first member at that member's default.
void TaggedUnionType::BuildDefault(void* out) const {
  std::memset(out, 0, Size());
  Members().front()->BuildDefault(out);
  SetTag(out, 0);
}

void TaggedUnionType::BitstreamWidth(const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::CountBits(
    const void*, const ConstIntegralView&, void*) const {
  NeverAskedOfATaggedUnion();
}

auto TaggedUnionType::WriteToStream(
    const void*, const IntegralView&, std::uint64_t) const -> std::uint64_t {
  NeverAskedOfATaggedUnion();
}

auto TaggedUnionType::ReadFromStream(
    const ConstIntegralView&, std::uint64_t, void*) const -> std::uint64_t {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::ResolveTriState(const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::ResolveWiredAnd(const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::ResolveWiredOr(const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::Dominating(const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::FilledLike(const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

auto TaggedUnionType::OrderBefore(const void*, const void*) const -> bool {
  NeverAskedOfATaggedUnion();
}

auto TaggedUnionType::IsTrue(const void*) const -> bool {
  NeverAskedOfATaggedUnion();
}

void TaggedUnionType::Reduce(Reduction, const void*, const void*, void*) const {
  NeverAskedOfATaggedUnion();
}

auto TaggedUnionType::Parts() const -> const PartsByPosition* {
  return nullptr;
}

auto TaggedUnionType::AsIntegral() const -> const IntegralValueType* {
  return nullptr;
}

auto TaggedUnionType::AsFixedArray() const -> const FixedArrayType* {
  return nullptr;
}

}  // namespace lyra::value
