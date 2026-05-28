module Top;
  real x = 2.5;
  initial begin
    if (x > 2.0)
      $display("greater");
    else
      $display("not greater");
  end
endmodule
