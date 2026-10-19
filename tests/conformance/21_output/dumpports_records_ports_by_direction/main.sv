// $dumpports writes an extended VCD file of the ports of the instances it
// names, or of the instance calling it where it names none: each instance a
// module scope named by its hierarchical name, and in it each port in the
// order the declaration gives them, its size 1 or its index range and its
// code an ascending number after a <. A value is a p, a state character for
// each position that reads it by who drives it -- D, U, N, Z into an input
// and L, H, X, T out of an output, and 0, 1, ?, F through an inout, whose
// driver is not told apart -- and its strengths toward 0 and 1, before
// its code; a real is an r and its value in %.16g. $dumpportsoff records every
// port as unknown in a section of its own (LRM 21.7.3.1, 21.7.3.2, 21.7.4.1,
// 21.7.4.2, 21.7.4.3).
module Dut (input logic clk, input logic [3:0] d, output logic [3:0] q,
            input real level, output logic [99:0] wide, output logic t,
            inout wire io);
  always_ff @(posedge clk) q <= d;
  assign wide = {96'h0, d};
  assign t = 1'bz;
  assign io = 1'bz;
endmodule

module Leaf (input logic a);
  initial $dumpports(, "leaf.evcd");
endmodule

module Top;
  logic clk = 0;
  logic [3:0] d = 4'h5;
  logic [3:0] q;
  real level = 1.5;
  logic [99:0] wide;
  logic t;
  wire io;
  Dut u (.clk(clk), .d(d), .q(q), .level(level), .wide(wide), .t(t), .io(io));
  Leaf l (.a(clk));

  string lines[$];
  string leaf_lines[$];

  task automatic Read(string file, output string into[$]);
    int fd;
    string line;
    into.delete();
    fd = $fopen(file, "r");
    while ($fgets(line, fd) > 0) begin
      if (line[line.len() - 1] == "\n") line = line.substr(0, line.len() - 2);
      into.push_back(line);
    end
    $fclose(fd);
  endtask

  // The value of the port `code` names under `#time`, as written before its
  // code: the p, its state characters and its strengths.
  function automatic string PortAt(int time_step, string code);
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

  // Whether `wanted` is one of the lines read, after the definitions or not.
  function automatic bit Has(string read[$], string wanted);
    foreach (read[i]) if (read[i] == wanted) return 1;
    return 0;
  endfunction

  initial begin
    $dumpports(Top.u, "ports.evcd");
    #1 clk = 1;
    #1 clk = 0;
    d = 4'ha;
    #1 clk = 1;
    #1 $dumpportsoff("ports.evcd");
    #1 $dumpportsflush;
    Read("ports.evcd", lines);
    Read("leaf.evcd", leaf_lines);
  end

  final begin
    if (!Has(lines, "$scope module Top.u $end"))
      $fatal(1, "the instance was not declared as the module scope Top.u");
    if (!Has(lines, "$var port 1 <0 clk $end"))
      $fatal(1, "clk was not declared first, as a 1-bit port of code <0");
    if (!Has(lines, "$var port [3:0] <1 d $end"))
      $fatal(1, "d was not declared second, as port [3:0] of code <1");
    if (!Has(lines, "$var port [3:0] <2 q $end"))
      $fatal(1, "q was not declared third, as port [3:0] of code <2");
    begin
      automatic string expected = "p";
      repeat (96) expected = {expected, "L"};
      expected = {expected, "LHLH 6 6"};
      if (!Has(lines, "$var port [99:0] <4 wide $end"))
        $fatal(1, "wide was not declared fifth, as port [99:0] of code <4");
      if (PortAt(0, "<4") != expected)
        $fatal(1, "wide was '%s' at time 0, expected %s", PortAt(0, "<4"),
               expected);
    end
    // A three-stated output and a three-stated inout, driven toward neither
    // value, in the characters of each one's direction.
    if (!Has(lines, "$var port 1 <5 t $end") || PortAt(0, "<5") != "pT 0 0")
      $fatal(1, "t was '%s' at time 0, expected pT 0 0", PortAt(0, "<5"));
    if (!Has(lines, "$var port 1 <6 io $end") || PortAt(0, "<6") != "pF 0 0")
      $fatal(1, "io was '%s' at time 0, expected pF 0 0", PortAt(0, "<6"));
    if (PortAt(0, "<3") != "r1.5")
      $fatal(1, "level was '%s' at time 0, expected r1.5", PortAt(0, "<3"));
    if (!Has(lines, "$dumpports"))
      $fatal(1, "the first values were not a $dumpports section");
    if (PortAt(0, "<0") != "pD 6 0")
      $fatal(1, "clk was '%s' at time 0, expected pD 6 0", PortAt(0, "<0"));
    if (PortAt(0, "<1") != "pDUDU 6 6")
      $fatal(1, "d was '%s' at time 0, expected pDUDU 6 6", PortAt(0, "<1"));
    if (PortAt(1, "<0") != "pU 0 6")
      $fatal(1, "clk was '%s' at time 1, expected pU 0 6", PortAt(1, "<0"));
    if (PortAt(0, "<2") != "pXXXX 6 6")
      $fatal(1, "q was '%s' at time 0, expected pXXXX 6 6", PortAt(0, "<2"));
    if (PortAt(1, "<2") != "pLHLH 6 6")
      $fatal(1, "q was '%s' at time 1, expected pLHLH 6 6", PortAt(1, "<2"));
    if (PortAt(3, "<2") != "pHLHL 6 6")
      $fatal(1, "q was '%s' at time 3, expected pHLHL 6 6", PortAt(3, "<2"));
    if (!Has(lines, "$dumpportsoff"))
      $fatal(1, "switching the dump off wrote no $dumpportsoff section");
    if (PortAt(4, "<1") != "pNNNN 6 6")
      $fatal(1, "d was '%s' when switched off, expected pNNNN 6 6",
             PortAt(4, "<1"));
    if (PortAt(4, "<3") != "pN 6 6")
      $fatal(1, "level was '%s' when switched off, expected pN 6 6",
             PortAt(4, "<3"));
    if (PortAt(4, "<2") != "pXXXX 6 6")
      $fatal(1, "q was '%s' when switched off, expected pXXXX 6 6",
             PortAt(4, "<2"));
    if (!Has(leaf_lines, "$scope module Top.l $end") ||
        !Has(leaf_lines, "$var port 1 <0 a $end"))
      $fatal(1, "a $dumpports naming no instance did not dump its caller");
    $display("All checks passed");
  end
endmodule
