`ifndef NUM_ITERS
`define NUM_ITERS 200000000
`endif

// Fixed non-recursive call chain of 16 functions.
// Each function does XOR/shift work that LLVM cannot constant-fold.
// The accumulator feeds back into the next call, preventing closed-form
// optimization of the loop.

module Top;
  localparam int NUM_ITERS = `NUM_ITERS;

  function automatic int f16(int x); return x ^ (x >> 16); endfunction
  function automatic int f15(int x); return f16(x ^ (x << 1)); endfunction
  function automatic int f14(int x); return f15(x + (x >> 2)); endfunction
  function automatic int f13(int x); return f14(x ^ (x << 3)); endfunction
  function automatic int f12(int x); return f13(x + (x >> 4)); endfunction
  function automatic int f11(int x); return f12(x ^ (x << 5)); endfunction
  function automatic int f10(int x); return f11(x + (x >> 6)); endfunction
  function automatic int f9(int x);  return f10(x ^ (x << 7)); endfunction
  function automatic int f8(int x);  return f9(x + (x >> 8));  endfunction
  function automatic int f7(int x);  return f8(x ^ (x << 1));  endfunction
  function automatic int f6(int x);  return f7(x + (x >> 2));  endfunction
  function automatic int f5(int x);  return f6(x ^ (x << 3));  endfunction
  function automatic int f4(int x);  return f5(x + (x >> 4));  endfunction
  function automatic int f3(int x);  return f4(x ^ (x << 5));  endfunction
  function automatic int f2(int x);  return f3(x + (x >> 6));  endfunction
  function automatic int f1(int x);  return f2(x ^ (x << 7));  endfunction

  initial begin
    int acc;
    acc = 32'hDEAD_BEEF;

    for (int i = 0; i < NUM_ITERS; i++) begin
      acc = f1(acc);
    end

    $display("call-chain done: acc=%0d", acc);
    $finish;
  end
endmodule
