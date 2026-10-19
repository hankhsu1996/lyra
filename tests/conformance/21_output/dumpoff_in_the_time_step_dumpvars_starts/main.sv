// $dumpvars starts the dump at the end of the time step it runs in, and a
// $dumpoff later in that same step still switches that dump off: the file
// records the first values and then the x checkpoint, both at that time, and
// no change until $dumpon (LRM 21.7.1.2, 21.7.1.3).
module Top;
  logic [3:0] a = 4'h6;

  string sections[$];
  string a_after_off;
  bit changed_while_off;

  initial begin
    int fd;
    string line;
    string code;
    int now;
    bit off;

    $dumpfile("started_off.vcd");
    $dumpvars;
    $dumpoff;
    #1 a = 4'h7;
    #1 $dumpflush;

    fd = $fopen("started_off.vcd", "r");
    now = -1;
    while ($fgets(line, fd) > 0) begin
      string kind, line_code, name;
      int size;
      int space;
      if (line[line.len() - 1] == "\n") line = line.substr(0, line.len() - 2);
      if (line.substr(0, 3) == "$var" &&
          $sscanf(line, "$var %s %d %s %s", kind, size, line_code, name) == 4)
      begin
        if (name == "a") code = line_code;
        continue;
      end
      if (line.len() > 1 && line[0] == "#") begin
        now = line.substr(1, line.len() - 1).atoi();
        continue;
      end
      if (now < 0) continue;
      if (line == "$dumpvars" || line == "$dumpoff") begin
        sections.push_back($sformatf("%0d %s", now, line));
        off = line == "$dumpoff";
        continue;
      end
      space = -1;
      for (int k = 0; k < line.len(); k++) if (line[k] == " ") space = k;
      if (space > 0 && line.substr(space + 1, line.len() - 1) == code) begin
        if (now > 0) changed_while_off = 1;
        else if (off) a_after_off = line.substr(0, space - 1);
      end
    end
    $fclose(fd);
  end

  final begin
    if (sections.size() != 2 || sections[0] != "0 $dumpvars" ||
        sections[1] != "0 $dumpoff")
      $fatal(1, "the sections were not $dumpvars then $dumpoff at time 0");
    if (a_after_off.len() < 2 || a_after_off[0] != "b")
      $fatal(1, "a was recorded at $dumpoff as '%s', expected x", a_after_off);
    for (int k = 1; k < a_after_off.len(); k++)
      if (a_after_off[k] != "x")
        $fatal(1, "a was recorded at $dumpoff as '%s', expected x",
               a_after_off);
    if (changed_while_off)
      $fatal(1, "a change was recorded after the dump was switched off");
    $display("All checks passed");
  end
endmodule
