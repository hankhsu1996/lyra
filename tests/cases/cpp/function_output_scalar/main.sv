module Top;
  int x;

  function void get_five(output int v);
    v = 5;
  endfunction

  initial begin
    x = 99;
    get_five(x);
  end
endmodule
