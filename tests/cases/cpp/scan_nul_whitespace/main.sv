module Top;
  // LRM 21.3.4.3(a): "For $sscanf, null characters shall also be considered
  // white space." The rule names $sscanf alone, so the same byte ends an
  // input field under $sscanf and is ordinary input under $fscanf. Both
  // halves run the same bytes through both functions to pin the difference.
  //
  // The $sscanf sources are unpacked byte arrays so the embedded NUL bytes
  // survive into the runtime, avoiding the backend's string-literal NUL
  // escape path.
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

  int fd;
  int count_file;
  string f1, f2;
  int f1_len;

  initial begin
    count_ints = $sscanf(ints_in, "%d %d", a_int, b_int);
    count_strs = $sscanf(strs_in, "%s %s", s1, s2);

    // The same "hello\0world" bytes reached through a file. $fscanf does not
    // break the field at the NUL, so the first %s takes all eleven bytes and
    // the second reaches end of file with nothing left to match, leaving its
    // output argument at the value it already held.
    fd = $fopen("nul_input.txt", "w");
    $fwrite(fd, "hello");
    $fwrite(fd, "%c", 8'h00);
    $fwrite(fd, "world");
    $fclose(fd);

    fd = $fopen("nul_input.txt", "r");
    count_file = $fscanf(fd, "%s %s", f1, f2);
    $fclose(fd);
    f1_len = f1.len();
  end
endmodule
