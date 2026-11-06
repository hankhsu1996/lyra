// A clocking block an interface declares is one copy per instance of the
// interface (LRM 14.7), and every way of reaching that instance reaches the
// same block (LRM 14.9, 25.5, 25.9): an interface port, a virtual interface
// whose modport lists the block, and a hierarchical name. Through each a
// process waits on the block's event, reads an input as last sampled, and
// drives an output, which lands in the Re-NBA region of the event it matures
// at whatever region set the driving thread runs in (LRM 14.16, 24.3.1). A
// drive's target, the virtual interface naming it included, is evaluated once,
// where the drive is reached, so the drive lands in the instance it named and
// after that instance's skew, wherever the handle points by then.
interface Bus(input logic clk);
  logic [7:0] d = 0;
  wire [7:0] w;
  wire [7:0] p;
  logic [7:0] v = 0;
  logic [7:0] e = 0;
  clocking cb @(posedge clk);
    input d;
    output w;
    output p;
    output v;
    output negedge e;
  endclocking
  modport tb(clocking cb);
  initial begin
    #1 d = 8'h07;
    #20 d = 8'h09;         // t = 21
  end
endinterface

// Two specializations of one interface, each with its own output skew, reached
// through a virtual interface of its own type: each drive is landed by the
// instance it names, after that instance's skew.
interface PBus #(parameter int D = 1) (input logic clk);
  logic [7:0] o = 0;
  clocking cb @(posedge clk);
    output #D o;
  endclocking
  modport tb(clocking cb);
endinterface

// Reads and drives a bus through a virtual interface.
class Driver;
  virtual Bus.tb vb;
  logic [7:0] read = 0;
  task run();
    @(vb.cb);              // 10
    read = vb.cb.d;
    vb.cb.w <= 8'h5a;      // lands at 10
    vb.cb.v[3:0] <= 4'h6;  // lands at 10, in bits 3:0 only
  endtask
endclass

// Drives a bus through a virtual interface whose handle, select and value it
// changes right after the drive.
class HandleOnceDriver;
  virtual Bus.tb vs[2];
  int picks = 0;
  function int pick();
    picks++;
    return 0;
  endfunction
  int k = 1;
  logic [3:0] nibble = 4'h4;
  task run_edge();
    @(vs[0].cb);           // 10
    // Matures at 30 and lands at the named bus's falling edge after it, 40,
    // in bits 7:4: the handle, the select and the value are those of here.
    vs[pick()].cb.e[k*4 +: 4] <= ##1 nibble;
    vs[0] = vs[1];
    k = 0;
    nibble = 4'hF;
  endtask
endclass

module User(Bus b);
  logic [7:0] read = 0;
  initial begin
    @(b.cb);               // 10
    @(b.cb);               // 30
    read = b.cb.d;
    b.cb.v[7:4] <= 4'h9;   // lands at 30, in bits 7:4 only
  end
endmodule

program Stimulus(Bus b);
  initial begin
    @(b.cb);               // 10
    b.cb.p <= ##1 8'h3c;   // lands at 30
    // The last program to end ends the run (LRM 24.3), so this one outlasts
    // the checks.
    repeat (3) @(b.cb);
  end
endprogram

module Top;
  logic clk = 0;
  always #10 clk = ~clk;   // rises at 10, 30, 50

  Bus bus(clk);
  // A second instance, on a clock falling at 14, 28, ...
  logic clk2 = 0;
  always #7 clk2 = ~clk2;
  Bus bus2(clk2);
  PBus #(.D(1)) quick(clk);
  PBus #(.D(3)) slow(clk);
  virtual PBus #(.D(1)).tb vq = quick;
  virtual PBus #(.D(3)).tb vs3 = slow;
  User user(bus);
  Stimulus stimulus(bus);
  Driver driver = new;
  HandleOnceDriver edge_driver = new;
  logic [7:0] quick_at_12 = 0, slow_at_12 = 8'hFF, slow_at_14 = 0;
  logic [7:0] hier_read = 0, w_at_11 = 0, p_at_29 = 0, p_at_31 = 0;
  logic [7:0] v_at_11 = 0, v_at_31 = 0;
  logic [7:0] e_at_39 = 8'hFF, e_at_41 = 0, other_e_at_41 = 8'hFF;
  bit done = 0;

  initial begin
    @(vq.cb);              // 10
    vq.cb.o <= 8'h11;      // lands at 11
    vs3.cb.o <= 8'h33;     // lands at 13
    #2 quick_at_12 = quick.o;
    slow_at_12 = slow.o;
    #2 slow_at_14 = slow.o;
  end

  initial begin
    edge_driver.vs[0] = bus;
    edge_driver.vs[1] = bus2;
    edge_driver.run_edge();
    #29 e_at_39 = bus.e;   // 39
    #2 e_at_41 = bus.e;    // 41
    other_e_at_41 = bus2.e;
  end

  initial begin
    driver.vb = bus;
    driver.run();
    #1 w_at_11 = bus.w;    // 11
    v_at_11 = bus.v;
    #18 p_at_29 = bus.p;   // 29
    @(bus.cb);             // 30
    hier_read = bus.cb.d;
    #1 p_at_31 = bus.p;    // 31
    v_at_31 = bus.v;
    #14 done = 1;          // 45, once the edge-skewed drive has landed
    $finish;
  end

  final begin
    if (!done) $fatal(1, "the processes did not all finish");
    if (driver.read !== 8'h07) $fatal(1, "through the virtual interface read %h, expected 07", driver.read);
    if (w_at_11 !== 8'h5a) $fatal(1, "the drive through the virtual interface left %h, expected 5a", w_at_11);
    if (user.read !== 8'h09) $fatal(1, "through the port read %h, expected 09", user.read);
    if (hier_read !== 8'h09) $fatal(1, "through the hierarchical name read %h, expected 09", hier_read);
    if (p_at_29 !== 8'hzz) $fatal(1, "the program's drive landed early: %h", p_at_29);
    if (p_at_31 !== 8'h3c) $fatal(1, "the program's drive left %h, expected 3c", p_at_31);
    if (v_at_11 !== 8'h06) $fatal(1, "the variable output at 11 was %h, expected 06", v_at_11);
    if (v_at_31 !== 8'h96) $fatal(1, "the variable output at 31 was %h, expected 96", v_at_31);
    if (quick_at_12 !== 8'h11) $fatal(1, "the #1 specialization left %h at 12, expected 11", quick_at_12);
    if (slow_at_12 !== 8'h00) $fatal(1, "the #3 specialization landed early: %h at 12", slow_at_12);
    if (slow_at_14 !== 8'h33) $fatal(1, "the #3 specialization left %h at 14, expected 33", slow_at_14);
    if (edge_driver.picks !== 1) $fatal(1, "the drive's handle was evaluated %0d times, expected 1", edge_driver.picks);
    if (e_at_39 !== 8'h00) $fatal(1, "the edge-skewed drive landed early: %h at 39", e_at_39);
    if (e_at_41 !== 8'h40) $fatal(1, "the edge-skewed drive left %h at 41, expected 40", e_at_41);
    if (other_e_at_41 !== 8'h00) $fatal(1, "the drive reached the other bus: %h", other_e_at_41);
    $display("All checks passed");
  end
endmodule
