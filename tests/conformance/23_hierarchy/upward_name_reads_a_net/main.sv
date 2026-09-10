// LRM 23.8 lists a net among the declarations a hierarchical name reaches, and
// LRM 23.6 lists reading one and waiting on it among the things such a name
// does -- neither of them qualified by the direction the name travelled. So a
// net named upward reads and triggers exactly as the same net named downward
// does, and what the reader gets hold of is the one resolution the net's
// drivers feed.
module Child;
  logic saw = 1'b0;
  int   woke = 0;

  initial begin
    #1;
    saw = Top.signalled;
  end

  always @(Top.counted) woke = woke + 1;
endmodule

module Top;
  wire signalled;
  wire [7:0] counted;
  logic [7:0] drive = 8'd0;

  assign signalled = 1'b1;
  assign counted   = drive;

  Child c ();

  initial begin
    #2;
    drive = 8'd5;
    #2;
    drive = 8'd9;
  end

  final begin
    if (c.saw !== 1'b1) $fatal(1, "an upward net read %b, expected 1", c.saw);
    if (c.woke !== 2) $fatal(1, "an upward net triggered %0d times, expected 2", c.woke);
    $display("All checks passed");
  end
endmodule
