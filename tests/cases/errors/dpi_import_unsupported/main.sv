module Top;
  import "DPI-C" function int c_add(int a, int b);
  int x;
  initial begin
    x = c_add(3, 4);
  end
endmodule
