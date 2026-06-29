module Top;
  string s;
  string sarr[2];
  int    calls;
  int    elem_calls;
  int    base_calls;
  string sarr0_after;

  function automatic int idx1();
    calls = calls + 1;
    return 1;
  endfunction

  function automatic int idx0();
    calls = calls + 1;
    return 0;
  endfunction

  initial begin
    s = "hello";
    sarr[0] = "world";

    calls = 0;
    s[idx1()] += 1;
    elem_calls = calls;

    calls = 0;
    sarr[idx0()][1] += 1;
    base_calls = calls;
    sarr0_after = sarr[0];
  end
endmodule
