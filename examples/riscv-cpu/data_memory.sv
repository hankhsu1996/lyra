module data_memory
  import riscv_package::*;
(
    input  logic     clk,
    input  address_t addr,
    input  word_t    write_data,
    input  logic     write_enable,
    output word_t    read_data
);

  localparam int Size = 256;
  word_t mem[Size];

  function automatic logic [7:0] addr_to_index(address_t a);
    return a[9:2];
  endfunction

  assign read_data = mem[addr_to_index(addr)];

  always @(posedge clk) begin
    if (write_enable) begin
      mem[addr_to_index(addr)] <= write_data;
    end
  end

endmodule
