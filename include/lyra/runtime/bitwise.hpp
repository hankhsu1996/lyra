#pragma once

#include <concepts>
#include <cstdint>
#include <span>
#include <type_traits>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/packed.hpp"
#include "lyra/runtime/packed_words.hpp"

namespace lyra::runtime {

namespace detail {

template <typename V>
concept BitViewLike = std::same_as<std::remove_cvref_t<V>, ConstBitView> ||
                      std::same_as<std::remove_cvref_t<V>, BitView>;

template <typename V>
concept LogicViewLike = std::same_as<std::remove_cvref_t<V>, ConstLogicView> ||
                        std::same_as<std::remove_cvref_t<V>, LogicView>;

inline auto ToConstView(ConstBitView v) -> ConstBitView {
  return v;
}
inline auto ToConstView(BitView v) -> ConstBitView {
  return v.AsConst();
}
inline auto ToConstView(ConstLogicView v) -> ConstLogicView {
  return v;
}
inline auto ToConstView(LogicView v) -> ConstLogicView {
  return v.AsConst();
}

inline auto NotPlaneWords(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> src,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = ~src[i];
  }
  MaskUnusedTopBits(dst, width);
}

inline auto AndPlaneWords(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> a,
    std::span<const std::uint64_t> b, std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = a[i] & b[i];
  }
  MaskUnusedTopBits(dst, width);
}

inline auto OrPlaneWords(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> a,
    std::span<const std::uint64_t> b, std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = a[i] | b[i];
  }
  MaskUnusedTopBits(dst, width);
}

inline auto XorPlaneWords(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> a,
    std::span<const std::uint64_t> b, std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = a[i] ^ b[i];
  }
  MaskUnusedTopBits(dst, width);
}

inline auto XnorPlaneWords(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> a,
    std::span<const std::uint64_t> b, std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = ~(a[i] ^ b[i]);
  }
  MaskUnusedTopBits(dst, width);
}

// 4-state encoding: s=0,v=0 -> 0; s=0,v=1 -> 1; s=1,v=0 -> Z; s=1,v=1 -> X.
// Per IEEE 1800: ~0 -> 1, ~1 -> 0, ~X -> X, ~Z -> X.
inline auto LogicNotWords(
    std::span<std::uint64_t> dst_v, std::span<std::uint64_t> dst_s,
    std::span<const std::uint64_t> src_v, std::span<const std::uint64_t> src_s,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    dst_v[i] = (~src_v[i]) | src_s[i];
    dst_s[i] = src_s[i];
  }
  MaskUnusedTopBits(dst_v, width);
  MaskUnusedTopBits(dst_s, width);
}

// SV table: 0&_=0; 1&1=1; otherwise X.
inline auto LogicAndWords(
    std::span<std::uint64_t> dst_v, std::span<std::uint64_t> dst_s,
    std::span<const std::uint64_t> av, std::span<const std::uint64_t> as,
    std::span<const std::uint64_t> bv, std::span<const std::uint64_t> bs,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t a_zero = ~as[i] & ~av[i];
    const std::uint64_t b_zero = ~bs[i] & ~bv[i];
    const std::uint64_t a_one = ~as[i] & av[i];
    const std::uint64_t b_one = ~bs[i] & bv[i];
    const std::uint64_t is_zero = a_zero | b_zero;
    const std::uint64_t is_one = a_one & b_one;
    const std::uint64_t new_s = ~is_zero & ~is_one;
    const std::uint64_t new_v = is_one | new_s;
    dst_v[i] = new_v;
    dst_s[i] = new_s;
  }
  MaskUnusedTopBits(dst_v, width);
  MaskUnusedTopBits(dst_s, width);
}

// SV table: 1|_=1; 0|0=0; otherwise X.
inline auto LogicOrWords(
    std::span<std::uint64_t> dst_v, std::span<std::uint64_t> dst_s,
    std::span<const std::uint64_t> av, std::span<const std::uint64_t> as,
    std::span<const std::uint64_t> bv, std::span<const std::uint64_t> bs,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t a_zero = ~as[i] & ~av[i];
    const std::uint64_t b_zero = ~bs[i] & ~bv[i];
    const std::uint64_t a_one = ~as[i] & av[i];
    const std::uint64_t b_one = ~bs[i] & bv[i];
    const std::uint64_t is_one = a_one | b_one;
    const std::uint64_t is_zero = a_zero & b_zero;
    const std::uint64_t new_s = ~is_zero & ~is_one;
    const std::uint64_t new_v = is_one | new_s;
    dst_v[i] = new_v;
    dst_s[i] = new_s;
  }
  MaskUnusedTopBits(dst_v, width);
  MaskUnusedTopBits(dst_s, width);
}

// Any X/Z -> X; else dst_v_bit = av ^ bv.
inline auto LogicXorWords(
    std::span<std::uint64_t> dst_v, std::span<std::uint64_t> dst_s,
    std::span<const std::uint64_t> av, std::span<const std::uint64_t> as,
    std::span<const std::uint64_t> bv, std::span<const std::uint64_t> bs,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t new_s = as[i] | bs[i];
    const std::uint64_t new_v = new_s | (av[i] ^ bv[i]);
    dst_v[i] = new_v;
    dst_s[i] = new_s;
  }
  MaskUnusedTopBits(dst_v, width);
  MaskUnusedTopBits(dst_s, width);
}

// Any X/Z -> X; else dst_v_bit = ~(av ^ bv).
inline auto LogicXnorWords(
    std::span<std::uint64_t> dst_v, std::span<std::uint64_t> dst_s,
    std::span<const std::uint64_t> av, std::span<const std::uint64_t> as,
    std::span<const std::uint64_t> bv, std::span<const std::uint64_t> bs,
    std::uint64_t width) -> void {
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t new_s = as[i] | bs[i];
    const std::uint64_t new_v = new_s | ~(av[i] ^ bv[i]);
    dst_v[i] = new_v;
    dst_s[i] = new_s;
  }
  MaskUnusedTopBits(dst_v, width);
  MaskUnusedTopBits(dst_s, width);
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseNotImpl(ConstBitView src) -> Bit<Shape, Signed> {
  if (src.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseNot: width mismatch");
  }
  Bit<Shape, Signed> out;
  NotPlaneWords(
      PlaneAccess::MutableValue(out), PlaneAccess::ValueWords(src),
      Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseNotImpl(ConstLogicView src) -> Logic<Shape, Signed> {
  if (src.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseNot: width mismatch");
  }
  Logic<Shape, Signed> out;
  LogicNotWords(
      PlaneAccess::MutableValue(out), PlaneAccess::MutableState(out),
      PlaneAccess::ValueWords(src), PlaneAccess::StateWords(src),
      Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseAndImpl(ConstBitView lhs, ConstBitView rhs) -> Bit<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseAnd: width mismatch");
  }
  Bit<Shape, Signed> out;
  AndPlaneWords(
      PlaneAccess::MutableValue(out), PlaneAccess::ValueWords(lhs),
      PlaneAccess::ValueWords(rhs), Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseAndImpl(ConstLogicView lhs, ConstLogicView rhs)
    -> Logic<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseAnd: width mismatch");
  }
  Logic<Shape, Signed> out;
  LogicAndWords(
      PlaneAccess::MutableValue(out), PlaneAccess::MutableState(out),
      PlaneAccess::ValueWords(lhs), PlaneAccess::StateWords(lhs),
      PlaneAccess::ValueWords(rhs), PlaneAccess::StateWords(rhs),
      Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseOrImpl(ConstBitView lhs, ConstBitView rhs) -> Bit<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseOr: width mismatch");
  }
  Bit<Shape, Signed> out;
  OrPlaneWords(
      PlaneAccess::MutableValue(out), PlaneAccess::ValueWords(lhs),
      PlaneAccess::ValueWords(rhs), Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseOrImpl(ConstLogicView lhs, ConstLogicView rhs)
    -> Logic<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseOr: width mismatch");
  }
  Logic<Shape, Signed> out;
  LogicOrWords(
      PlaneAccess::MutableValue(out), PlaneAccess::MutableState(out),
      PlaneAccess::ValueWords(lhs), PlaneAccess::StateWords(lhs),
      PlaneAccess::ValueWords(rhs), PlaneAccess::StateWords(rhs),
      Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseXorImpl(ConstBitView lhs, ConstBitView rhs) -> Bit<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseXor: width mismatch");
  }
  Bit<Shape, Signed> out;
  XorPlaneWords(
      PlaneAccess::MutableValue(out), PlaneAccess::ValueWords(lhs),
      PlaneAccess::ValueWords(rhs), Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseXorImpl(ConstLogicView lhs, ConstLogicView rhs)
    -> Logic<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseXor: width mismatch");
  }
  Logic<Shape, Signed> out;
  LogicXorWords(
      PlaneAccess::MutableValue(out), PlaneAccess::MutableState(out),
      PlaneAccess::ValueWords(lhs), PlaneAccess::StateWords(lhs),
      PlaneAccess::ValueWords(rhs), PlaneAccess::StateWords(rhs),
      Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseXnorImpl(ConstBitView lhs, ConstBitView rhs) -> Bit<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseXnor: width mismatch");
  }
  Bit<Shape, Signed> out;
  XnorPlaneWords(
      PlaneAccess::MutableValue(out), PlaneAccess::ValueWords(lhs),
      PlaneAccess::ValueWords(rhs), Shape.TotalWidth());
  return out;
}

template <PackedShape Shape, Signedness Signed>
auto BitwiseXnorImpl(ConstLogicView lhs, ConstLogicView rhs)
    -> Logic<Shape, Signed> {
  if (lhs.Width() != Shape.TotalWidth() || rhs.Width() != Shape.TotalWidth()) {
    throw InternalError("BitwiseXnor: width mismatch");
  }
  Logic<Shape, Signed> out;
  LogicXnorWords(
      PlaneAccess::MutableValue(out), PlaneAccess::MutableState(out),
      PlaneAccess::ValueWords(lhs), PlaneAccess::StateWords(lhs),
      PlaneAccess::ValueWords(rhs), PlaneAccess::StateWords(rhs),
      Shape.TotalWidth());
  return out;
}

}  // namespace detail

template <PackedShape Shape, Signedness Signed, detail::BitViewLike V>
auto BitwiseNot(V src) -> Bit<Shape, Signed> {
  return detail::BitwiseNotImpl<Shape, Signed>(detail::ToConstView(src));
}
template <PackedShape Shape, Signedness Signed, detail::LogicViewLike V>
auto BitwiseNot(V src) -> Logic<Shape, Signed> {
  return detail::BitwiseNotImpl<Shape, Signed>(detail::ToConstView(src));
}

template <
    PackedShape Shape, Signedness Signed, detail::BitViewLike L,
    detail::BitViewLike R>
auto BitwiseAnd(L lhs, R rhs) -> Bit<Shape, Signed> {
  return detail::BitwiseAndImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}
template <
    PackedShape Shape, Signedness Signed, detail::LogicViewLike L,
    detail::LogicViewLike R>
auto BitwiseAnd(L lhs, R rhs) -> Logic<Shape, Signed> {
  return detail::BitwiseAndImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}

template <
    PackedShape Shape, Signedness Signed, detail::BitViewLike L,
    detail::BitViewLike R>
auto BitwiseOr(L lhs, R rhs) -> Bit<Shape, Signed> {
  return detail::BitwiseOrImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}
template <
    PackedShape Shape, Signedness Signed, detail::LogicViewLike L,
    detail::LogicViewLike R>
auto BitwiseOr(L lhs, R rhs) -> Logic<Shape, Signed> {
  return detail::BitwiseOrImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}

template <
    PackedShape Shape, Signedness Signed, detail::BitViewLike L,
    detail::BitViewLike R>
auto BitwiseXor(L lhs, R rhs) -> Bit<Shape, Signed> {
  return detail::BitwiseXorImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}
template <
    PackedShape Shape, Signedness Signed, detail::LogicViewLike L,
    detail::LogicViewLike R>
auto BitwiseXor(L lhs, R rhs) -> Logic<Shape, Signed> {
  return detail::BitwiseXorImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}

template <
    PackedShape Shape, Signedness Signed, detail::BitViewLike L,
    detail::BitViewLike R>
auto BitwiseXnor(L lhs, R rhs) -> Bit<Shape, Signed> {
  return detail::BitwiseXnorImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}
template <
    PackedShape Shape, Signedness Signed, detail::LogicViewLike L,
    detail::LogicViewLike R>
auto BitwiseXnor(L lhs, R rhs) -> Logic<Shape, Signed> {
  return detail::BitwiseXnorImpl<Shape, Signed>(
      detail::ToConstView(lhs), detail::ToConstView(rhs));
}

}  // namespace lyra::runtime
