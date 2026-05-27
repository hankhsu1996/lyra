module Top;
  logic signed_neg_match;
  logic signed_pos_match;
  logic mixed_unsigned_zero_ext;
  logic signed_with_wildcard;
  initial begin
    begin
      logic signed [7:0] a;
      logic signed [15:0] b;
      a = -1;
      b = -1;
      signed_neg_match = a ==? b;
    end
    begin
      logic signed [7:0] a;
      logic signed [15:0] b;
      a = 8'sd5;
      b = 16'sd5;
      signed_pos_match = a ==? b;
    end
    begin
      logic [7:0] a;
      logic signed [15:0] b;
      a = 8'd255;
      b = -1;
      mixed_unsigned_zero_ext = a ==? b;
    end
    begin
      logic signed [7:0] a;
      logic signed [15:0] b;
      a = -1;
      b = 16'sb1111_1111_zzzz_zzzz;
      signed_with_wildcard = a ==? b;
    end
  end
endmodule
