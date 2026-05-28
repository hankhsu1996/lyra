module Top;
  real x = 0.0;
  initial begin
    if (x)
      $display("nonzero");
    else
      $display("zero");
  end
endmodule
