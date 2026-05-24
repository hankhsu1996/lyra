module Top;
  int out;
  initial begin
    int flag_a;
    int flag_b;
    flag_a = 0;
    flag_b = 1;
    if (flag_a)
      out = 10;
    else if (flag_b)
      out = 22;
    else
      out = 30;
  end
endmodule
