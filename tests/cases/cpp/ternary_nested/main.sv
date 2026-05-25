module Top;
  int a;
  int b;
  int c;
  int max_abc;
  initial begin
    a = 4;
    b = 9;
    c = 6;
    max_abc = (a > b)
                ? ((a > c) ? a : c)
                : ((b > c) ? b : c);
  end
endmodule
