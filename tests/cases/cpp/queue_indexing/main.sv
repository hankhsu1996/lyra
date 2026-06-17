module Top;
  int q [$] = '{10, 20, 30};
  int a;
  int b;
  int oob;

  initial begin
    a = q[0];
    q[1] = 99;
    b = q[1];
    oob = q[5];
  end
endmodule
