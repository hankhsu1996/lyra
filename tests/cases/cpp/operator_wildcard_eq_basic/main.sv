module Top;
  logic match_z_wildcard;
  logic no_match_z_wildcard;
  logic match_x_wildcard;
  logic match_question_wildcard;
  logic all_wildcard_pattern;
  logic ne_no_match;
  logic ne_match;
  logic state2_eq_match;
  logic state2_ne_no_match;
  logic width_unsigned;
  initial begin
    logic [3:0] a;
    a = 4'b1011; match_z_wildcard = a ==? 4'b10zz;
    a = 4'b0011; no_match_z_wildcard = a ==? 4'b10zz;
    a = 4'b1011; match_x_wildcard = a ==? 4'b10xx;
    a = 4'b1011; match_question_wildcard = a ==? 4'b10??;
    a = 4'b1010; all_wildcard_pattern = a ==? 4'bzzzz;
    a = 4'b1011; ne_no_match = a !=? 4'b10zz;
    a = 4'b0011; ne_match = a !=? 4'b10zz;
    begin
      int x;
      int y;
      x = 5;
      y = 5;
      state2_eq_match = x ==? y;
    end
    begin
      int x;
      int y;
      x = 5;
      y = 6;
      state2_ne_no_match = x !=? y;
    end
    begin
      logic [3:0] s;
      int big;
      s = 4'd5;
      big = 32'd5;
      width_unsigned = s ==? big;
    end
  end
endmodule
