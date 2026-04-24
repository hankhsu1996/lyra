module register_file
  import riscv_package::*;
(
    input  logic              clk,
    input  logic              write_enable,
    input  register_address_t destination,
    input  register_address_t source1,
    input  register_address_t source2,
    input  word_t             write_data,
    output word_t             read_data1,
    output word_t             read_data2
);

  word_t registers[32];

  always_comb begin
    read_data1 = (source1 == '0) ? '0 : registers[source1];
    read_data2 = (source2 == '0) ? '0 : registers[source2];
  end

  always @(posedge clk) begin
    if (write_enable && destination != '0) begin
      registers[destination] <= write_data;
    end
  end

endmodule
