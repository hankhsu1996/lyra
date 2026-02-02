#pragma once

#include <expected>
#include <span>
#include <utility>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"

namespace llvm {
class Value;
}

namespace lyra::lowering::mir_to_llvm {

class Context;

// Lower a string-like argument to a string handle.
// - For string type: returns {handle, false} (no release needed)
// - For packed type: converts via EmitPackedToString, returns {handle, true}
// - Otherwise: returns error
//
// Caller MUST call LyraStringRelease on the handle if needs_release is true.
// Prefer WithStringHandle() to avoid manual release boilerplate.
auto LowerArgAsStringHandle(
    Context& context, const mir::Operand& operand, TypeId type_id)
    -> Result<std::pair<llvm::Value*, bool>>;

// Execute body(handle) with automatic release of temporary handles.
// - Calls LowerArgAsStringHandle to get (handle, needs_release)
// - Invokes body(handle) which returns Result<void>
// - If needs_release, emits LyraStringRelease (even if body failed)
// - Returns body's result
//
// Usage:
//   return WithStringHandle(ctx, op, type, [&](llvm::Value* h) -> Result<void>
//   {
//     builder.CreateCall(SomeRuntime, {h, ...});
//     return {};
//   });
//
// Note: body must NOT store the handle beyond its scope.
// Release is emitted unconditionally before returning, ensuring no leaks
// even when body returns an error.
template <class F>
auto WithStringHandle(
    Context& context, const mir::Operand& operand, TypeId type_id, F&& body)
    -> Result<void>;

// Validate all format ops can be lowered (for early-exit before
// LyraStringFormatStart). Returns error for unsupported types, 4-state values,
// or values wider than 64 bits.
auto ValidateFormatOps(Context& context, std::span<const mir::FormatOp> ops)
    -> Result<void>;

// Lower a single FormatOp to a string format buffer.
// Handles kLiteral, kString, and value formatting.
// Caller must have already called LyraStringFormatStart to create buf.
// Returns error only for unsupported types (rare after ValidateFormatOps).
auto LowerFormatOpToBuffer(
    Context& context, llvm::Value* buf, const mir::FormatOp& op)
    -> Result<void>;

// Template implementation (must be in header)
template <class F>
auto WithStringHandle(
    Context& context, const mir::Operand& operand, TypeId type_id, F&& body)
    -> Result<void> {
  auto handle_result = LowerArgAsStringHandle(context, operand, type_id);
  if (!handle_result) return std::unexpected(handle_result.error());
  auto [handle, needs_release] = *handle_result;

  // Call body, then release handle (even on error path)
  auto body_result = body(handle);

  if (needs_release) {
    context.GetBuilder().CreateCall(context.GetLyraStringRelease(), {handle});
  }

  return body_result;
}

}  // namespace lyra::lowering::mir_to_llvm
