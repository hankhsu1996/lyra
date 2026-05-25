module Top;
  int sel;
  int a;
  int b;
  int taken;
  initial begin
    sel = 0;
    a = 4;
    b = 11;
    taken = 0;
    if ((sel ? a : b) > 5) begin
      taken = 1;
    end
  end
endmodule
