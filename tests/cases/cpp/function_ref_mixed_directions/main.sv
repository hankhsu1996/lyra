module Top;
  int o;
  int r;

  function automatic void compute(output int o_arg, ref int r_arg, input int v);
    o_arg = v * 2;
    r_arg = r_arg + v;
  endfunction

  initial begin
    r = 100;
    compute(o, r, 5);
  end
endmodule
