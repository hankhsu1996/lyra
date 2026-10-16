#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of an untagged unpacked union (LRM 7.3) as code compiled before any
// design existed is handed it: one storage its members overlay, each at its
// start. Its members own nothing, so copying a union copies its bytes and
// ending one does nothing; whether one changed is whether its bytes did, since
// those are what a read through any member sees. Every other operation on the
// whole value is its first member's, read as that member's type holds a value
// (LRM 7.3.2), as LRM 21.2.1.6 and 11.4.14.1 have printing and streaming read
// that member.
//
// Generated code states one of these for each union type it hands a value of
// to such code, as constant data of this class, so its layout is part of what
// the two sides agree on.
class UnionType final : public ValueType {
 public:
  UnionType(
      std::uint32_t size, std::uint32_t align,
      std::span<const ValueType* const> members);

  [[nodiscard]] auto Members() const -> std::span<const ValueType* const> {
    return {members_, count_};
  }

  // Where the type states its own parts, which code stating a type as constant
  // data places them by.
  [[nodiscard]] auto CountStatedAt() const -> StatedAt;
  [[nodiscard]] auto MembersStatedAt() const -> StatedAt;

  void Copy(const void* value, void* out) const override;
  void Move(void* value, void* out) const noexcept override;
  void Destroy(void* value) const noexcept override;
  void Assign(void* storage, const void* value) const override;

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override;
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override;
  void ClearAboveWidths(void* value) const override;

  void BuildDefault(void* out) const override;

  void BitstreamWidth(const void* value, void* out) const override;
  void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const override;
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t override;
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t override;

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override;
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override;
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override;

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override;
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override;

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override;
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override;
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* override;

 private:
  [[nodiscard]] auto First() const -> const ValueType&;

  std::uint32_t count_;
  const ValueType* const* members_;
};

// The type of a tagged union (LRM 7.3.2, 11.9) as code compiled before any
// design existed is handed it: its members overlaid as an untagged union's
// are, then the tag naming the member it holds, which is what copying, moving,
// ending and comparing it follow. A copy carries every byte over and then the
// member the tag names, as that member's type copies it. A tag naming no
// member is bits another member of an untagged union holding this one wrote,
// and a union held so owns nothing (LRM 7.3), so its bytes are all there is of
// it.
class TaggedUnionType final : public ValueType {
 public:
  TaggedUnionType(
      std::uint32_t size, std::uint32_t align,
      std::span<const ValueType* const> members, std::uint32_t tag_offset,
      std::uint32_t tag_bytes);

  [[nodiscard]] auto Members() const -> std::span<const ValueType* const> {
    return {members_, count_};
  }

  // The tag of the tagged union laid out at `value`.
  [[nodiscard]] auto Tag(const void* value) const -> std::uint64_t;

  [[nodiscard]] auto CountStatedAt() const -> StatedAt;
  [[nodiscard]] auto MembersStatedAt() const -> StatedAt;
  [[nodiscard]] auto TagOffsetStatedAt() const -> StatedAt;
  [[nodiscard]] auto TagBytesStatedAt() const -> StatedAt;

  void Copy(const void* value, void* out) const override;
  void Move(void* value, void* out) const noexcept override;
  void Destroy(void* value) const noexcept override;
  void Assign(void* storage, const void* value) const override;

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override;
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override;
  void ClearAboveWidths(void* value) const override;

  void BuildDefault(void* out) const override;

  void BitstreamWidth(const void* value, void* out) const override;
  void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const override;
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t override;
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t override;

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override;
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override;
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override;

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override;
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override;

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override;
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override;
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* override;

 private:
  // The type of the member the tagged union at `value` holds, and nothing
  // where its tag names none.
  [[nodiscard]] auto Held(const void* value) const -> const ValueType*;
  void SetTag(void* value, std::uint64_t tag) const;

  std::uint32_t count_;
  const ValueType* const* members_;
  std::uint32_t tag_offset_;
  std::uint32_t tag_bytes_;
};

}  // namespace lyra::value
