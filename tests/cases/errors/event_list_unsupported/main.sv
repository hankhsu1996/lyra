module Top;
  logic a;
  logic b;
  initial begin
    @(a or b);
  end
endmodule
