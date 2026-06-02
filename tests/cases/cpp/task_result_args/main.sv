module Top;
  int x;
  int y;
  int z;

  task my_task(input int a, input int b, inout int c, output int d, output int e);
    c = a;
    d = b;
    e = c;
  endtask

  initial begin
    x = 0;
    y = 0;
    z = 0;
    my_task(7, 9, x, y, z);
  end
endmodule
