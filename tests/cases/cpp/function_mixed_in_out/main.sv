module Top;
  int x;
  int y;

  function void f(input int a, output int b, inout int c);
    b = a * 2;
    c = c + a;
  endfunction

  initial begin
    x = 0;
    y = 100;
    f(5, x, y);
  end
endmodule
