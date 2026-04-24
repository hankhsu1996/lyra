module instr_memory
  import riscv_package::*;
#(
    parameter string PROGRAM = "programs/sum_1_to_10.hex"
)(
    input  address_t addr,
    output word_t    data
);

  localparam int Size = 256;
  word_t mem[Size];

  function automatic logic [7:0] addr_to_index(address_t a);
    return a[9:2];
  endfunction

  initial begin
    $readmemh(PROGRAM, mem);
  end

  assign data = mem[addr_to_index(addr)];

endmodule
