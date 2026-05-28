module Top;
  real x = 1.5;
  initial begin
    if (x)
      $display("nonzero");
    else
      $display("zero");
  end
endmodule
