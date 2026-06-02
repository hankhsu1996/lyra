module Top;
  int x;

  function void inc(inout int v);
    v = v + 1;
  endfunction

  initial begin
    x = 10;
    inc(x);
  end
endmodule
