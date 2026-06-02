module Top;
  int r_int;
  string r_str;
  int r_arr;

  function int probe_int;
    int x;
    return x;
  endfunction

  function string probe_str;
    string s;
    return s;
  endfunction

  function int probe_arr;
    int a[3];
    return a[1];
  endfunction

  initial begin
    r_int = probe_int();
    r_str = probe_str();
    r_arr = probe_arr();
  end
endmodule
