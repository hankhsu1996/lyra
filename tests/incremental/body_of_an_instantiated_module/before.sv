// @remakes: Child
//
// A module's procedure assigns another value and its ports stay as they were,
// so the module that instantiates it is connected to the same thing.

module Child (
    input  logic [7:0] in,
    output logic [7:0] out
);
  always_comb out = in + 8'd1;
endmodule

module Parent;
  logic [7:0] given;
  logic [7:0] taken;
  Child child (
      .in (given),
      .out(taken)
  );
endmodule

module Top;
  Parent parent ();
endmodule
