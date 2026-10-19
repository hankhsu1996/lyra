// A dump records the static variables of named blocks, tasks and functions in
// scopes of their own, declared as begin, fork, task and function (LRM
// 21.7.2.1 `scope_type`), and records a vector of any width, past the 64 bits
// a machine word holds as well as within them: a variable, a net and a port
// (LRM 21.7.1.2, 21.7.2.2). The codes are the tool's to choose, so they are
// read off the declarations.
module Leaf (input logic [99:0] in);
endmodule

module Top;
  logic [99:0] wide = 100'h1;
  wire [99:0] wide_net = wide;
  Leaf u (.in(wide));

  task bump;
    static logic [3:0] count = 4'h0;
    count = count + 1;
  endtask

  function int twice(int x);
    static int calls = 0;
    calls = calls + 1;
    return 2 * x;
  endfunction

  string lines[$];
  string scopes[$];
  string wide_at_1, net_at_1, port_at_1, count_at_1, held_at_1;

  // What the last line under `#time` records for `code`, before the space.
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

  initial begin : body
    static logic [3:0] held = 4'h9;
    int fd;
    string line;
    string path[$];
    string codes[string];
    int ignored;

    $dumpfile("blocks.vcd");
    $dumpvars;
    fork : side
      static logic forked = 1'b1;
    join
    #1 bump;
    ignored = twice(3);
    held = 4'h6;
    wide = {4'ha, 96'h0} | 100'h5;
    #1 $dumpflush;

    fd = $fopen("blocks.vcd", "r");
    while ($fgets(line, fd) > 0) begin
      string keyword, kind, name, code;
      int size;
      if (line[line.len() - 1] == "\n") line = line.substr(0, line.len() - 2);
      lines.push_back(line);
      if ($sscanf(line, "%s", keyword) != 1) continue;
      if (keyword == "$scope" &&
          $sscanf(line, "$scope %s %s", kind, name) == 2) begin
        path.push_back(name);
        scopes.push_back({kind, " ", name});
      end
      else if (keyword == "$upscope")
        void'(path.pop_back());
      else if (keyword == "$var" &&
               $sscanf(line, "$var %s %d %s %s", kind, size, code, name) == 4)
      begin
        automatic string full = "";
        foreach (path[i]) full = {full, path[i], "."};
        codes[{full, name}] = code;
      end
    end
    $fclose(fd);

    wide_at_1 = RecordedAt(1, codes["Top.wide"]);
    net_at_1 = RecordedAt(1, codes["Top.wide_net"]);
    port_at_1 = RecordedAt(1, codes["Top.u.in"]);
    count_at_1 = RecordedAt(1, codes["Top.bump.count"]);
    held_at_1 = RecordedAt(1, codes["Top.body.held"]);
  end

  final begin
    logic [99:0] vector;
    logic [3:0] narrow;

    if (!("task bump" inside {scopes}))
      $fatal(1, "the task was not declared as the task scope bump");
    if (!("function twice" inside {scopes}))
      $fatal(1, "the function was not declared as the function scope twice");
    if (!("begin body" inside {scopes}))
      $fatal(1, "the named block was not declared as the begin scope body");
    if (!("fork side" inside {scopes}))
      $fatal(1, "the named fork was not declared as the fork scope side");

    narrow = 'x;
    if ($sscanf(count_at_1, "b%b", narrow) != 1 || narrow !== 4'b0001)
      $fatal(1, "bump.count was recorded as '%s', expected 0001", count_at_1);
    narrow = 'x;
    if ($sscanf(held_at_1, "b%b", narrow) != 1 || narrow !== 4'b0110)
      $fatal(1, "body.held was recorded as '%s', expected 0110", held_at_1);

    vector = 'x;
    if ($sscanf(wide_at_1, "b%b", vector) != 1 ||
        vector !== ({4'ha, 96'h0} | 100'h5))
      $fatal(1, "wide was recorded as '%s'", wide_at_1);
    vector = 'x;
    if ($sscanf(net_at_1, "b%b", vector) != 1 ||
        vector !== ({4'ha, 96'h0} | 100'h5))
      $fatal(1, "wide_net was recorded as '%s'", net_at_1);
    if (port_at_1 != wide_at_1)
      $fatal(1, "u.in was recorded as '%s', unlike what it is connected to",
             port_at_1);
    $display("All checks passed");
  end
endmodule
