module Top;
  int x;
  int y;

  function int clamp(input int v);
    clamp = v;
    if (v > 10) return 10;
    clamp = clamp + 100;
  endfunction

  initial begin
    x = clamp(50);
    y = clamp(3);
  end
endmodule
