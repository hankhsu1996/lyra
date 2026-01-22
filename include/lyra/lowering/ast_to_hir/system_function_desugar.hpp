#pragma once

#include <optional>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

// Classification of pure system functions that should be desugared
// into core HIR nodes (not kept as SystemCall).
enum class PureSysFnKind {
  // Category 1: Value conversion -> kCast
  kSigned,    // $signed(x)
  kUnsigned,  // $unsigned(x)
  kItor,      // $itor(i) - integer to real
  kRtoi,      // $rtoi(r) - real to integer

  // Category 2: Bit reinterpretation -> kBitCast
  kRealToBits,       // $realtobits(r) - real -> bit[63:0]
  kBitsToReal,       // $bitstoreal(b) - bit[63:0] -> real
  kShortRealToBits,  // $shortrealtobits(sr) - shortreal -> bit[31:0]
  kBitsToShortReal,  // $bitstoshortreal(b) - bit[31:0] -> shortreal
};

// Classify a system call as a pure function that should be desugared.
// Returns nullopt if the call is not a pure system function.
auto ClassifyPureSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<PureSysFnKind>;

// Lower a pure system function to core HIR nodes.
// This function should only be called after ClassifyPureSystemFunction
// returns a valid kind.
auto LowerPureSystemFunction(
    const slang::ast::CallExpression& call, PureSysFnKind kind,
    SymbolRegistrar& registrar, Context* ctx) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
