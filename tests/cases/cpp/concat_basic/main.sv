module Top;
  byte a;
  byte b;
  bit [3:0] n0;
  bit [3:0] n1;
  bit [15:0] two_bytes;
  bit [11:0] three_nibbles;
  bit [11:0] mixed_widths;
  bit [7:0] single_operand;
  bit [15:0] with_literal;

  initial begin
    a = 8'hAB;
    b = 8'hCD;
    n0 = 4'hF;
    n1 = 4'h3;
    two_bytes = {a, b};
    three_nibbles = {n0, n1, 4'hA};
    mixed_widths = {n0, b};
    single_operand = {a};
    with_literal = {a, 8'h34};
  end
endmodule
