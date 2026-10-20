// $dumpoff makes a checkpoint in which every dumped variable is recorded as x,
// and no change is recorded until $dumpon, which records each one's value at
// that time; $dumpall records every value as it stands, changed or not. Each
// checkpoint is a section opened by its keyword and closed by $end (LRM
// 21.7.1.3, 21.7.1.4, 21.7.2.3). The codes are the tool's to choose, so they
// are read off the declarations.
module Top;
  logic [3:0] a = 4'h1;
  logic [3:0] b = 4'h9;

  string lines[$];

  string off_keyword, on_keyword, all_keyword;
  string a_at_2, a_at_3, a_at_4, a_at_6, b_at_6;

  // The code of the $var line declaring `name`.
  function automatic string CodeOf(string name);
    foreach (lines[i]) begin
      string kind, code, line_name;
      int size;
      if (lines[i].substr(0, 3) == "$var" &&
          $sscanf(lines[i], "$var %s %d %s %s", kind, size, code,
                  line_name) == 4 &&
          line_name == name)
        return code;
    end
    return "";
  endfunction

  // What the last line under `#time` records for `code`: everything before
  // the code, which for a vector ends at the space before it.
  function automatic string RecordedAt(int time_step, string code);
    int now = -1;
    string found = "";
    foreach (lines[i]) begin
      string line = lines[i];
      int space = -1;
      if (line.len() > 1 && line[0] == "#") begin
        now = line.substr(1, line.len() - 1).atoi();
        continue;
      end
      if (now != time_step) continue;
      for (int k = 0; k < line.len(); k++) if (line[k] == " ") space = k;
      if (space > 0 && line.substr(space + 1, line.len() - 1) == code)
        found = line.substr(0, space - 1);
    end
    return found;
  endfunction

  // The keyword opening a section under `#time`, none where there is none.
  function automatic string SectionAt(int time_step);
    int now = -1;
    foreach (lines[i]) begin
      string line = lines[i];
      if (line.len() > 1 && line[0] == "#") begin
        now = line.substr(1, line.len() - 1).atoi();
        continue;
      end
      if (now == time_step && line.len() > 1 && line[0] == "$" &&
          line != "$end")
        return line;
    end
    return "";
  endfunction

  // Whether `recorded` is a vector every position of which is x: written
  // whole, or shortened to the x that extends to its left (LRM 21.7.2.2).
  function automatic bit AllUnknown(string recorded);
    if (recorded.len() < 2 || recorded[0] != "b") return 0;
    for (int k = 1; k < recorded.len(); k++)
      if (recorded[k] != "x") return 0;
    return 1;
  endfunction

  initial begin
    int fd;
    string line;
    string a_code, b_code;

    $dumpfile("checkpoints.vcd");
    $dumpvars;
    #2 $dumpoff;
    #1 a = 4'h3;
    #1 $dumpon;
    #2 $dumpall;
    #1 $dumpflush;

    fd = $fopen("checkpoints.vcd", "r");
    while ($fgets(line, fd) > 0) begin
      if (line[line.len() - 1] == "\n") line = line.substr(0, line.len() - 2);
      lines.push_back(line);
    end
    $fclose(fd);

    a_code = CodeOf("a");
    b_code = CodeOf("b");
    off_keyword = SectionAt(2);
    on_keyword = SectionAt(4);
    all_keyword = SectionAt(6);
    a_at_2 = RecordedAt(2, a_code);
    a_at_3 = RecordedAt(3, a_code);
    a_at_4 = RecordedAt(4, a_code);
    a_at_6 = RecordedAt(6, a_code);
    b_at_6 = RecordedAt(6, b_code);
  end

  final begin
    logic [3:0] vector;

    if (off_keyword != "$dumpoff")
      $fatal(1, "time 2 opened '%s', expected $dumpoff", off_keyword);
    if (!AllUnknown(a_at_2))
      $fatal(1, "a was recorded at $dumpoff as '%s', expected x", a_at_2);
    if (a_at_3 != "")
      $fatal(1, "a was recorded as '%s' while the dump was off", a_at_3);
    if (on_keyword != "$dumpon")
      $fatal(1, "time 4 opened '%s', expected $dumpon", on_keyword);
    vector = 'x;
    if ($sscanf(a_at_4, "b%b", vector) != 1 || vector !== 4'b0011)
      $fatal(1, "a was recorded at $dumpon as '%s', expected 0011", a_at_4);
    if (all_keyword != "$dumpall")
      $fatal(1, "time 6 opened '%s', expected $dumpall", all_keyword);
    vector = 'x;
    if ($sscanf(a_at_6, "b%b", vector) != 1 || vector !== 4'b0011)
      $fatal(1, "a was recorded at $dumpall as '%s', expected 0011", a_at_6);
    vector = 'x;
    if ($sscanf(b_at_6, "b%b", vector) != 1 || vector !== 4'b1001)
      $fatal(1, "b was recorded at $dumpall as '%s', expected 1001", b_at_6);
    $display("All checks passed");
  end
endmodule
