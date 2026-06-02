module Top;
  int a;
  int b;

  function int g(output int o);
    o = 5;
    return 3;
  endfunction

  initial begin
    a = b + g(b);
  end
endmodule
