// RISC-V Register File
// 32 registers, x0 hardwired to zero
// 2 read ports, 1 write port

import riscv_pkg::*;

module regfile (
    input  logic      clk,
    input  logic      we,
    input  reg_addr_t rd,
    input  reg_addr_t rs1,
    input  reg_addr_t rs2,
    input  word_t     wdata,
    output word_t     rdata1,
    output word_t     rdata2
);

  // Register array
  word_t regs[32];

  // Initialize all registers to zero
  initial begin
    foreach (regs[i]) regs[i] = '0;
  end

  // Combinational reads (x0 hardwired to zero)
  always_comb begin
    rdata1 = (rs1 == '0) ? '0 : regs[rs1];
    rdata2 = (rs2 == '0) ? '0 : regs[rs2];
  end

  // Synchronous write (x0 is hardwired to zero)
  // Use 'always' instead of 'always_ff' to allow initial block coexistence
  always @(posedge clk) begin
    if (we && rd != '0) begin
      regs[rd] <= wdata;
    end
  end

endmodule
