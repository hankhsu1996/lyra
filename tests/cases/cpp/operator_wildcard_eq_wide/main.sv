module Top;
  logic wide_80_match;
  logic wide_80_no_match;
  logic wide_128_match;
  logic wide_80_x_result;
  initial begin
    begin
      logic [79:0] a;
      a = 80'hAAAA_FFFFFFFFFFFFFFFF;
      wide_80_match = a ==? 80'hAAAA_zzzzzzzzzzzzzzzz;
    end
    begin
      logic [79:0] a;
      a = 80'hBBBB_FFFFFFFFFFFFFFFF;
      wide_80_no_match = a ==? 80'hAAAA_zzzzzzzzzzzzzzzz;
    end
    begin
      logic [127:0] a;
      a = 128'hAAAA_BBBB_CCCC_DDDD_EEEE_FFFF_00112233;
      wide_128_match = a ==? 128'hAAAA_BBBB_CCCC_DDDD_zzzzzzzz_zzzzzzzz;
    end
    begin
      logic [79:0] a;
      a = 80'hAAAX_0000000000000000;
      wide_80_x_result = a ==? 80'hAAAA_zzzzzzzzzzzzzzzz;
    end
  end
endmodule
