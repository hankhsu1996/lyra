module Top;
  byte a;
  byte b;
  byte c;

  initial begin
    // Replication inside a destructuring LHS is illegal (LRM 11.4.12.1).
    {{2{a}}, b} = 24'h112233;
    c = b;
  end
endmodule
