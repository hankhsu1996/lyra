module Top;
  initial begin
    int flag_a;
    int flag_b;
    int out;
    flag_a = 0;
    flag_b = 1;
    if (flag_a)
      out = 10;
    else if (flag_b)
      out = 22;
    else
      out = 30;
    $display("out=%0d", out);
  end
endmodule
