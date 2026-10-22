// $dumpvars with no arguments dumps every variable of the design to the file
// $dumpfile names. The file declares each one by a $var line giving its kind,
// its size and the code its values are written under, then records the value
// each holds at the end of the time step $dumpvars ran in, then every change
// after, under a #time line naming the step (LRM 21.7.1.1, 21.7.1.2, 21.7.2).
// A vector is written after a b, a real after an r, and a triggered event as a
// 1 directly before its code; the codes themselves are the tool's to choose,
// so they are read off the declarations. An event is recorded as triggered in
// a step the object it names then is triggered, so one given another event's
// object follows that object and leaves the one it named before (LRM 15.5.5.1).
module Top;
  logic [3:0] a = 4'h3;
  wire [3:0] w = a;
  real r = 1.5;
  integer n = 7;
  event e;
  event moved, target, left_behind;

  string lines[$];

  string a_kind, w_kind, r_kind, n_kind, e_kind;
  int a_size, w_size, n_size;
  string a_at_0, w_at_0, r_at_0, n_at_0;
  string a_at_5, w_at_5, r_at_5, e_at_5, n_at_5;
  string moved_at_6, moved_at_7;

  // The kind, size and code of the $var line declaring `name`.
  function automatic void Declared(string name, output string kind,
                                   output int size, output string code);
    kind = "";
    size = -1;
    code = "";
    foreach (lines[i]) begin
      string line_kind, line_code, line_name;
      int line_size;
      if (lines[i].substr(0, 3) == "$var" &&
          $sscanf(lines[i], "$var %s %d %s %s", line_kind, line_size,
                  line_code, line_name) == 4 &&
          line_name == name) begin
        kind = line_kind;
        size = line_size;
        code = line_code;
      end
    end
  endfunction

  // What the last line under `#time` records for `code`: everything before
  // the code, which for a vector or a real ends at the space before it.
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
      else if (space < 0 && line.len() > 1 &&
               line.substr(1, line.len() - 1) == code)
        found = line.substr(0, 0);
    end
    return found;
  endfunction

  initial begin
    int fd;
    string line;
    string a_code, w_code, r_code, n_code, e_code, moved_code, moved_kind;
    int ignored;

    $dumpfile("changes.vcd");
    $dumpvars;
    #5 a = 4'h5;
    r = 2.25;
    n = -2;
    -> e;
    left_behind = moved;
    moved = target;
    #1 -> target;
    #1 -> left_behind;
    #1 $dumpflush;

    fd = $fopen("changes.vcd", "r");
    while ($fgets(line, fd) > 0) begin
      if (line[line.len() - 1] == "\n") line = line.substr(0, line.len() - 2);
      lines.push_back(line);
    end
    $fclose(fd);

    Declared("a", a_kind, a_size, a_code);
    Declared("w", w_kind, w_size, w_code);
    Declared("r", r_kind, ignored, r_code);
    Declared("n", n_kind, n_size, n_code);
    Declared("e", e_kind, ignored, e_code);
    Declared("moved", moved_kind, ignored, moved_code);
    a_at_0 = RecordedAt(0, a_code);
    w_at_0 = RecordedAt(0, w_code);
    r_at_0 = RecordedAt(0, r_code);
    n_at_0 = RecordedAt(0, n_code);
    a_at_5 = RecordedAt(5, a_code);
    w_at_5 = RecordedAt(5, w_code);
    r_at_5 = RecordedAt(5, r_code);
    n_at_5 = RecordedAt(5, n_code);
    e_at_5 = RecordedAt(5, e_code);
    moved_at_6 = RecordedAt(6, moved_code);
    moved_at_7 = RecordedAt(7, moved_code);
  end

  final begin
    logic [3:0] vector;
    logic [31:0] word;

    if (a_kind != "reg" || a_size !== 4)
      $fatal(1, "a was declared as '%s' of size %0d, expected reg of 4",
             a_kind, a_size);
    if (w_kind != "wire" || w_size !== 4)
      $fatal(1, "w was declared as '%s' of size %0d, expected wire of 4",
             w_kind, w_size);
    if (r_kind != "real")
      $fatal(1, "r was declared as '%s', expected real", r_kind);
    if (n_kind != "integer" || n_size !== 32)
      $fatal(1, "n was declared as '%s' of size %0d, expected integer of 32",
             n_kind, n_size);
    if (e_kind != "event")
      $fatal(1, "e was declared as '%s', expected event", e_kind);

    vector = 'x;
    if ($sscanf(a_at_0, "b%b", vector) != 1 || vector !== 4'b0011)
      $fatal(1, "a was recorded at time 0 as '%s', expected 0011", a_at_0);
    vector = 'x;
    if ($sscanf(w_at_0, "b%b", vector) != 1 || vector !== 4'b0011)
      $fatal(1, "w was recorded at time 0 as '%s', expected 0011", w_at_0);
    if (r_at_0.substr(0, 0) != "r" ||
        r_at_0.substr(1, r_at_0.len() - 1).atoreal() != 1.5)
      $fatal(1, "r was recorded at time 0 as '%s', expected 1.5", r_at_0);
    word = 'x;
    if ($sscanf(n_at_0, "b%b", word) != 1 || word !== 32'd7)
      $fatal(1, "n was recorded at time 0 as '%s', expected 7", n_at_0);

    vector = 'x;
    if ($sscanf(a_at_5, "b%b", vector) != 1 || vector !== 4'b0101)
      $fatal(1, "a was recorded at time 5 as '%s', expected 0101", a_at_5);
    vector = 'x;
    if ($sscanf(w_at_5, "b%b", vector) != 1 || vector !== 4'b0101)
      $fatal(1, "w was recorded at time 5 as '%s', expected 0101", w_at_5);
    if (r_at_5.substr(0, 0) != "r" ||
        r_at_5.substr(1, r_at_5.len() - 1).atoreal() != 2.25)
      $fatal(1, "r was recorded at time 5 as '%s', expected 2.25", r_at_5);
    word = 'x;
    if ($sscanf(n_at_5, "b%b", word) != 1 || word !== -32'sd2)
      $fatal(1, "n was recorded at time 5 as '%s', expected -2", n_at_5);
    if (e_at_5 != "1")
      $fatal(1, "e was recorded at time 5 as '%s', expected it triggered",
             e_at_5);
    if (moved_at_6 != "1")
      $fatal(1, "an event given another's object was recorded as '%s' when that object was triggered, expected it triggered",
             moved_at_6);
    if (moved_at_7 != "")
      $fatal(1, "an event given another's object was recorded as '%s' when the object it left was triggered, expected nothing",
             moved_at_7);
    $display("All checks passed");
  end
endmodule
