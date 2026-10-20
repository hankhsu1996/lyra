// $dumpvars names what it dumps: a level count of 1 takes the variables of the
// instance named and none below it, a count of 0 takes every instance below
// it as well, and a variable named alone is taken by itself. A generate block
// is part of the instance holding it rather than a level below it, and is
// declared as a begin scope where an instance is a module one. Several calls
// in one time step add up. With no $dumpfile the file is dump.vcd (LRM
// 21.7.1.1, 21.7.1.2, 21.7.2.1). Each declaration's hierarchical name is read
// off the $scope and $upscope lines around its $var line (LRM 21.7.2.3).
module Leaf;
  logic x = 1'b1;
endmodule

module Mid;
  logic m = 1'b0;
  Leaf l ();
  if (1) begin : gen
    logic g = 1'b1;
  end
endmodule

module Top;
  logic t = 1'b1;
  logic skipped = 1'b0;
  Mid u ();
  Mid v ();

  string declared[$];
  string scopes[$];

  initial begin
    int fd;
    string line;
    string path[$];

    $dumpvars(1, Top.u);
    $dumpvars(0, Top.v);
    $dumpvars(0, Top.t);
    #1 $dumpflush;

    fd = $fopen("dump.vcd", "r");
    while ($fgets(line, fd) > 0) begin
      string keyword, kind, name, code;
      int size;
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
        declared.push_back({full, name});
      end
      else if (keyword == "$enddefinitions")
        break;
    end
    $fclose(fd);
  end

  final begin
    automatic string expected[$] = '{"Top.t", "Top.u.gen.g", "Top.u.m",
                                     "Top.v.gen.g", "Top.v.l.x", "Top.v.m"};
    automatic string sorted[$] = declared;
    sorted.sort();
    if (sorted != expected) begin
      automatic string spelled = "";
      foreach (sorted[i]) spelled = {spelled, " ", sorted[i]};
      $fatal(1, "the dump declared%s, expected Top.t Top.u.gen.g Top.u.m %s",
             spelled, "Top.v.gen.g Top.v.l.x Top.v.m");
    end
    foreach (scopes[i]) begin
      if (scopes[i] == "module gen")
        $fatal(1, "the generate block gen was declared as a module scope");
      if (scopes[i] == "begin u" || scopes[i] == "begin v")
        $fatal(1, "the instance %s was declared as a begin scope", scopes[i]);
    end
    if (!(scopes.size() > 0 && "begin gen" inside {scopes}))
      $fatal(1, "the generate block gen was not declared as a begin scope");
    $display("All checks passed");
  end
endmodule
