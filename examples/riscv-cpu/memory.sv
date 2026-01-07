// RISC-V Memory
// Fixed-size memory for simulation
// Combined instruction and data memory

import riscv_pkg::*;

module memory (
    input  logic  clk,
    input  addr_t iaddr,      // Instruction address
    output word_t idata,      // Instruction data
    input  addr_t daddr,      // Data address
    input  word_t wdata,      // Write data
    input  logic  we,         // Write enable
    output word_t rdata       // Read data
);

  // Fixed-size memory (1KB = 256 words)
  localparam int MemSize = 256;
  word_t mem[MemSize];

  // Convert byte address to word index
  function automatic logic [7:0] addr_to_idx(addr_t addr);
    return addr[9:2];  // Word-aligned, use bits [9:2] for 256-word memory
  endfunction

  // Initialize from file
  initial begin
    foreach (mem[i]) mem[i] = '0;
    $readmemh("program.hex", mem);
  end

  // Instruction read (combinational)
  always_comb begin
    idata = mem[addr_to_idx(iaddr)];
  end

  // Data read (combinational)
  always_comb begin
    rdata = mem[addr_to_idx(daddr)];
  end

  // Data write (synchronous)
  // Use 'always' instead of 'always_ff' to allow initial block coexistence
  always @(posedge clk) begin
    if (we) begin
      mem[addr_to_idx(daddr)] <= wdata;
    end
  end

endmodule
