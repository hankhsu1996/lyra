module Top;
  realtime a = 1.5;
  realtime b = 2.5;
  realtime c;
  initial begin
    c = a + b;
    $display("%f", c);
  end
endmodule
