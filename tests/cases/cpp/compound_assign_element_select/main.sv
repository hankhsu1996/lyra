module Top;
  logic [7:0] set_bit_const;
  logic [7:0] clear_bit_const;
  logic [7:0] set_bit_dynamic;
  logic [15:0] add_dynamic_range;

  initial begin
    set_bit_const = 8'b00000000;
    set_bit_const[3] |= 1'b1;

    clear_bit_const = 8'b11111111;
    clear_bit_const[5] &= 1'b0;

    set_bit_dynamic = 8'b00000000;
    set_bit_dynamic[3] |= 1'b1;

    add_dynamic_range = 16'h0000;
    add_dynamic_range[4 +: 4] += 4'd5;
  end
endmodule
