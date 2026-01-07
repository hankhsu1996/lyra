// RISC-V CPU Testbench
// Simple test program: compute sum of 1 to 10

import riscv_pkg::*;

module tb;

  logic clk = 0;
  logic rst_n = 0;

  // Clock generation
  always #5 clk = ~clk;

  // CPU instance
  cpu u_cpu (
      .clk  (clk),
      .rst_n(rst_n)
  );

  // Test program: compute sum of 1..10
  // Result should be 55 in x3
  //
  // Assembly:
  //   addi x1, x0, 0      # sum = 0
  //   addi x2, x0, 1      # i = 1
  //   addi x4, x0, 11     # limit = 11
  // loop:
  //   add  x1, x1, x2     # sum += i
  //   addi x2, x2, 1      # i++
  //   bne  x2, x4, loop   # if i != 11 goto loop
  //   add  x3, x1, x0     # x3 = sum (result)
  //   jal  x0, end        # halt (infinite loop)
  // end:
  //   jal  x0, end

  initial begin
    // Program is loaded from program.hex via $readmemh in memory module

    // Reset sequence
    #20 rst_n = 1;

    // Run for enough cycles
    repeat (100) @(posedge clk);

    // Check result
    $display("Test: Sum of 1..10");
    $display("x1 (sum) = %0d", u_cpu.u_regfile.regs[1]);
    $display("x2 (i)   = %0d", u_cpu.u_regfile.regs[2]);
    $display("x3 (res) = %0d", u_cpu.u_regfile.regs[3]);
    $display("x4 (lim) = %0d", u_cpu.u_regfile.regs[4]);

    if (u_cpu.u_regfile.regs[3] == 55)
      $display("PASS: Result is 55");
    else
      $display("FAIL: Expected 55, got %0d", u_cpu.u_regfile.regs[3]);

    $finish;
  end

  // Trace execution (optional, for debugging)
  always @(posedge clk) begin
    if (rst_n) begin
      $display("PC=%h INST=%h", u_cpu.pc, u_cpu.instruction);
    end
  end

endmodule
