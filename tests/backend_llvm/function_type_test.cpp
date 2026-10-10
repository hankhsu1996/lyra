#include <gtest/gtest.h>
#include <optional>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::backend::llvm_backend {
namespace {

// The machine type a callee is declared at takes, in order, what leads the
// operands, each operand with what its mode adds and the part the callee names
// where the arrangement puts it, what the callee is told of types, and the
// storage an answer is built in; and it returns what the arrangement says.
TEST(FunctionTypeTest, TheTypeIsWhatTheArrangementHandsOver) {
  const lir::CompilationUnit unit;
  CodeGenModule module(unit);
  llvm::LLVMContext& ctx = module.Context();
  llvm::Type* const ptr = module.Types().Ptr();
  llvm::Type* const i64 = llvm::Type::getInt64Ty(ctx);
  llvm::Type* const i1 = llvm::Type::getInt1Ty(ctx);
  llvm::Type* const f64 = llvm::Type::getDoubleTy(ctx);
  const lir::TypeId any{};

  const FnAbi abi{
      .implicit_arg = HeldWidthArg{.of = any},
      .named_part = NamedPartArg{.before = 1, .position = 3},
      .args =
          {ArgAbi{.type = ptr, .mode = PassDirect{}},
           ArgAbi{.type = ptr, .mode = PassWithExtent{.type = any}},
           ArgAbi{.type = ptr, .mode = PassWithShape{.type = any}},
           ArgAbi{.type = ptr, .mode = PassWithType{.type = any}},
           ArgAbi{.type = ptr, .mode = PassWithWidth{.type = any}},
           ArgAbi{.type = f64, .mode = PassDirect{}}},
      .type_args =
          {TypeConstantArg{.type = any}, TypeExtentArg{.type = any},
           TypeWidthArg{.type = any}},
      .ret = ReturnIndirect{.returned = i64}};
  const std::vector<llvm::Type*> expected{i64, ptr, i64, ptr, i64, i1,  ptr,
                                          i64, i1,  i1,  ptr, ptr, ptr, i64,
                                          f64, ptr, i64, i1,  i64, ptr};
  const llvm::FunctionType* const type = module.Types().GetFunctionType(abi);
  EXPECT_EQ(type->getReturnType(), i64);
  EXPECT_EQ(std::vector<llvm::Type*>(type->params()), expected);

  const llvm::FunctionType* const direct = module.Types().GetFunctionType(
      CallArranger::Arrange(
          {CallArranger::Direct(i64)}, ReturnDirect{.type = f64}));
  EXPECT_EQ(direct->getReturnType(), f64);
  EXPECT_EQ(
      std::vector<llvm::Type*>(direct->params()),
      std::vector<llvm::Type*>{i64});
}

// A name is declared at one type: an arrangement that gives another for a name
// already declared is refused, and the same one answers the same function.
TEST(FunctionTypeTest, ANameDeclaredAtTwoTypesIsRefused) {
  const lir::CompilationUnit unit;
  CodeGenModule module(unit);
  llvm::Type* const ptr = module.Types().Ptr();
  const FnAbi one = CallArranger::Arrange(
      {CallArranger::Direct(ptr)}, ReturnDirect{.type = ptr});
  const FnAbi other = CallArranger::Arrange(
      {CallArranger::Direct(ptr), CallArranger::Direct(ptr)},
      ReturnDirect{.type = ptr});
  const llvm::Value* const declared =
      module.RuntimeFunction("lyra_rt_probe", one).getCallee();
  EXPECT_EQ(module.RuntimeFunction("lyra_rt_probe", one).getCallee(), declared);
  EXPECT_THROW(module.RuntimeFunction("lyra_rt_probe", other), InternalError);
}

}  // namespace
}  // namespace lyra::backend::llvm_backend
