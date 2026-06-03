module Top;
  // LRM 21.3.4.3 Table 21-7 `%b`: per-character to one bit, with the
  // 4-state vocabulary 0/1/x/X/z/Z/?/_ from the spec.
  int code_basic, code_mixed;
  bit   [3:0] a_basic;
  logic [3:0] a_mixed;
  initial begin
    code_basic = $sscanf("1010", "%b", a_basic);
    code_mixed = $sscanf("1x0z", "%b", a_mixed);
    $display("a_basic=%b a_mixed=%b", a_basic, a_mixed);
  end
endmodule
