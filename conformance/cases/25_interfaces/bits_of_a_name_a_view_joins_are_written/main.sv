// A name a modport joins declarations under is a vector of the type of its
// expression (LRM 25.5.4), whose bits and parts a write may select at a position
// the program computes (LRM 11.5.1). Each bit written lands in the declaration
// that bit lies in, a bit outside the name writes nothing, and the position and
// the handle are evaluated once, for a nonblocking write where the statement is
// reached (LRM 10.4.2). A name standing for one part of one declaration has its
// bits selected in the range the name was given. A constant select of a joined
// net may be forced and released, as a constant select of a net is, only its
// bits in range taken over (LRM 10.6.2, 11.5.1).
interface Bus;
  logic [3:0] hi = 0, lo = 0;
  bit   [3:0] two = 0;
  logic [7:0] data = 0;
  logic signed [3:0] negative = -1;
  wire  [3:0] upper, lower;
  logic [3:0] left, right;
  logic [99:0] wide_high = 0, wide_low = 0;
  typedef struct packed { logic [1:0] x, y; } pair_t;
  pair_t      pair = 0;
  logic [3:0] clocked_high = 0, clocked_low = 0;
  int         at = 0;

  modport ctrl(
    output .Pair({hi, lo}),
    output .Mixed({two, hi}),
    output .Wires({upper, lower}),
    output .Driven({left, right}),
    output .Middle(data[5:2]),
    output .Split({data[7:6], data[1:0]}),
    output .Wide({wide_high, wide_low}),
    output .Packed({pair, hi}),
    output .Unsigned({negative}),
    output .Clocked({clocked_high, clocked_low}));
endinterface

module Sink(output logic [3:0] o);
  assign o = 4'b1001;
endmodule

module WritesThroughThePort(Bus.ctrl p);
  logic [1:0] rest;
  int         low = 2;
  integer     unknown = 'x;
  int         calls = 0;

  function automatic int next();
    calls++;
    return low;
  endfunction

  task automatic produce(output logic [3:0] made);
    made = 4'b0110;
  endtask

  task automatic reset();
    p.Pair = 8'h00;
    p.Mixed = 8'h00;
    p.Split = 4'h0;
    p.Middle = 4'h0;
  endtask

  assign p.Wires[5:2] = 4'b1001;

  logic tick = 0;
  always_ff @(posedge tick) p.Clocked[low + 2] <= 1'b1;

  task automatic run();
    low = 3;
    tick = 1;
    #1 if (p.Clocked !== 8'h20) $fatal(1, "a clocked nonblocking write");

    reset();
    p.Pair[2:1] = 2'b11;
    if (p.Pair !== 8'h06) $fatal(1, "a part inside one declaration");
    reset();
    p.Pair[5:2] = 4'b1001;
    if (p.Pair !== 8'b0010_0100) $fatal(1, "a part across two declarations");
    reset();
    p.Pair[6] = 1'b1;
    if (p.Pair !== 8'h40) $fatal(1, "a constant bit");

    reset();
    low = 3;
    p.Pair[low] = 1'b1;
    low = 4;
    p.Pair[low] = 1'b1;
    if (p.Pair !== 8'h18) $fatal(1, "a bit at a computed position");

    reset();
    low = 2;
    p.Pair[low +: 4] = 4'b1001;
    if (p.Pair !== 8'b0010_0100) $fatal(1, "an ascending part");
    reset();
    p.Pair[low + 3 -: 4] = 4'b0110;
    if (p.Pair !== 8'b0001_1000) $fatal(1, "a descending part");

    // LRM 11.5.1: only the bits in range are written.
    reset();
    low = -2;
    p.Pair[low +: 4] = 4'b1001;
    if (p.Pair !== 8'h02) $fatal(1, "a part below the name");
    reset();
    low = 6;
    p.Pair[low +: 4] = 4'b1001;
    if (p.Pair !== 8'b0100_0000) $fatal(1, "a part above the name");
    reset();
    p.Pair[9:6] = 4'b0110;
    if (p.Pair !== 8'b1000_0000) $fatal(1, "a constant part above the name");

    force p.Wires[5:4] = 2'b00;
    #1 if (Top.a.upper !== 4'bzz00) $fatal(1, "a forced part of a joined net");
    release p.Wires[5:4];
    #1 if (Top.a.upper !== 4'bzz10) $fatal(1, "a released part of a joined net");
    force p.Wires[9:6] = 4'b0001;
    #1 if (Top.a.upper !== 4'b01_10) $fatal(1, "a forced part reaching past the name");
    release p.Wires[9:6];
    #1 if (Top.a.upper !== 4'bzz10) $fatal(1, "a released part reaching past the name");

    reset();
    low = 9;
    p.Pair[low +: 4] = 4'b1111;
    if (p.Pair !== 8'h00) $fatal(1, "a part outside the name");
    reset();
    p.Pair[unknown +: 4] = 4'b1111;
    p.Pair[unknown] = 1'b1;
    if (p.Pair !== 8'h00) $fatal(1, "a position holding x");

    reset();
    calls = 0;
    low = 2;
    p.Pair[next() +: 4] = 4'b1111;
    if (calls !== 1 || p.Pair !== 8'h3c) $fatal(1, "evaluated once by a write");

    reset();
    p.Pair = 8'b0010_1100;
    calls = 0;
    p.Pair[next() +: 4] += 4'b0001;
    if (calls !== 1 || p.Pair !== 8'b0011_0000)
      $fatal(1, "evaluated once by an assignment operator");
    p.Pair = 8'b0000_1100;
    p.Pair[3:2]++;
    if (p.Pair !== 8'h00) $fatal(1, "an increment wraps in the part");
    p.Pair = 8'b0011_1100;
    if (p.Pair[5:2]-- !== 4'b1111 || p.Pair !== 8'b0011_1000)
      $fatal(1, "a decrement answers what the part held");

    // A declaration of two states takes the bits a part of four writes.
    reset();
    p.Mixed[5:2] = 4'b1x01;
    if (p.Mixed !== 8'b0010_0100) $fatal(1, "an x among the bits");

    reset();
    p.Pair[5:2] = 4'hf;
    p.Pair[3:2] <= 2'b00;
    if (p.Pair !== 8'h3c) $fatal(1, "a nonblocking write lands now");
    low = 4;
    p.Pair[low +: 2] <= 2'b00;
    low = 0;
    #1;
    if (p.Pair !== 8'h00) $fatal(1, "a nonblocking write at a position");

    reset();
    low = 3;
    p.Pair[low +: 2] <= #2 2'b11;
    low = 0;
    #1 if (p.Pair !== 8'h00) $fatal(1, "a delayed write lands early");
    #2 if (p.Pair !== 8'h18) $fatal(1, "a delayed write");

    reset();
    p.Pair[5:2] = #1 4'b1111;
    if (p.Pair !== 8'h3c) $fatal(1, "an intra-assignment delay");

    reset();
    produce(p.Pair[5:2]);
    if (p.Pair !== 8'b0001_1000) $fatal(1, "an output actual");

    reset();
    {<<{p.Pair[5:2]}} = 4'b0001;
    if (p.Pair !== 8'b0010_0000) $fatal(1, "a stream target");
    reset();
    {p.Pair[4:3], rest} = 4'b1001;
    if (p.Pair !== 8'b0001_0000 || rest !== 2'b01) $fatal(1, "a concatenation");
    reset();
    if ($sscanf("9", "%d", p.Pair[5:2]) !== 1 || p.Pair !== 8'b0010_0100)
      $fatal(1, "a system task's output");

    reset();
    p.Split[2:1] = 2'b11;
    if (p.Split !== 4'b0110) $fatal(1, "bits of parts of one declaration");
    reset();
    p.Middle[4:3] = 2'b11;
    if (p.Middle !== 4'b0110) $fatal(1, "a name with a range of its own");
    low = 2;
    p.Middle[low] = 1'b1;
    if (p.Middle !== 4'b0111) $fatal(1, "a computed bit in a name's range");

    low = 98;
    p.Wide[low +: 4] = 4'b1001;
    if (p.Wide[101:98] !== 4'b1001) $fatal(1, "a part across wide declarations");
    p.Wide[150:60] = '1;
    if (p.Wide !== {49'b0, {91{1'b1}}, 60'b0}) $fatal(1, "a wide part");
    reset();
    p.Packed[5:2] = 4'b1001;
    if (p.Packed !== 8'b0010_0100) $fatal(1, "a part of a packed structure");

    if (p.Unsigned < 0) $fatal(1, "a concatenation of one is unsigned");
    p.Unsigned[3] = 1'b0;
    if (p.Unsigned !== 4'b0111) $fatal(1, "bits of a concatenation of one");
  endtask
endmodule

module DrivesAPort(Bus.ctrl p);
  Sink inner(.o(p.Driven[5:2]));
endmodule

module Top;
  Bus a(), b(), c();
  WritesThroughThePort port(a);
  DrivesAPort driven(c);

  virtual Bus.ctrl views[2] = '{a, b};
  virtual Bus.ctrl view = a;
  int calls = 0;

  function automatic int pick();
    calls++;
    return 1;
  endfunction

  task automatic reset();
    a.hi = 0;
    a.lo = 0;
    b.hi = 0;
    b.lo = 0;
  endtask

  initial begin
    #1;
    if (a.upper !== 4'bzz10 || a.lower !== 4'b01zz)
      $fatal(1, "a continuous assignment drives only the bits it names");
    if ({c.left[1:0], c.right[3:2]} !== 4'b1001)
      $fatal(1, "an output port connection");
    port.run();

    reset();
    view.Pair[5:2] = 4'b1001;
    if ({a.hi, a.lo} !== 8'b0010_0100) $fatal(1, "a part through a handle");

    reset();
    calls = 0;
    views[pick()].Pair[5:2] = 4'b1111;
    if (calls !== 1 || {b.hi, b.lo} !== 8'h3c)
      $fatal(1, "the handle evaluated once");

    reset();
    calls = 0;
    views[pick()].Pair[5:2] += 4'b0001;
    if (calls !== 1 || {b.hi, b.lo} !== 8'h04)
      $fatal(1, "the handle evaluated once by an assignment operator");

    reset();
    a.at = 2;
    view.Pair[a.at +: 4] <= 4'b1111;
    view = b;
    a.at = 0;
    #1;
    if ({a.hi, a.lo} !== 8'h3c || {b.hi, b.lo} !== 8'h00)
      $fatal(1, "an update lands on the instance held at the statement");

    $display("All checks passed");
  end
endmodule
