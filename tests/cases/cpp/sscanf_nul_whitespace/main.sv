module Top;
  // LRM 21.3.4.3(a): for $sscanf, null characters shall also be considered
  // white space. Two scenarios: an integer pair split by NUL and a string
  // pair split by NUL. Sources are unpacked byte arrays so the embedded
  // NUL bytes survive into the runtime (avoiding the backend's string-
  // literal NUL escape path).
  // "42\09900" -- '4','2',NUL,'9','9','0','0'
  byte ints_in[0:6] = '{8'h34, 8'h32, 8'h00, 8'h39, 8'h39, 8'h30, 8'h30};
  int a_int, b_int;
  int count_ints;
  // "hello\0world" -- 'h','e','l','l','o',NUL,'w','o','r','l','d'
  byte strs_in[0:10] = '{
      8'h68, 8'h65, 8'h6C, 8'h6C, 8'h6F,
      8'h00,
      8'h77, 8'h6F, 8'h72, 8'h6C, 8'h64};
  string s1, s2;
  int count_strs;
  initial begin
    count_ints = $sscanf(ints_in, "%d %d", a_int, b_int);
    count_strs = $sscanf(strs_in, "%s %s", s1, s2);
  end
endmodule
