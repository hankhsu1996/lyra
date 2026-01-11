module writeback
  import riscv_package::*;
(
    input  opcode_t  opcode,
    input  logic     jump,
    input  logic     memory_to_register,
    input  address_t pc_plus_4,
    input  word_t    memory_data,
    input  word_t    immediate,
    input  word_t    alu_result,
    output word_t    write_data
);

  always_comb begin
    if (jump)
      write_data = pc_plus_4;
    else if (memory_to_register)
      write_data = memory_data;
    else if (opcode == OP_LUI)
      write_data = immediate;
    else
      write_data = alu_result;
  end

endmodule
