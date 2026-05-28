module Top;
  realtime x;
  initial begin
    x = 1.25;
    $display("%f", x);
  end
endmodule
