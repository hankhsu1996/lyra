module Top;
  logic a;
  logic b;
  logic y;
  initial begin
    y = a && b;
    y = a || b;
    y = a -> b;
    y = a <-> b;
  end
endmodule
