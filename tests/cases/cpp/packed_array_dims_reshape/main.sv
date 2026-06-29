// A flat vector assigned to a packed-of-packed of the same width is
// assignment-compatible in SystemVerilog (LRM 6.22.2) -- the front end draws
// no conversion for the dims-only difference, so the reshape happens at the
// store boundary. Both a declaration initializer and a procedural assignment
// must land the flat bits under the declared dimension stack, so an element
// select reads the right byte.
module Top;
  bit [3:0][7:0] init_reshape = 32'hAABBCCDD;
  bit [7:0] init_b3, init_b2, init_b1, init_b0;

  bit [31:0] flat = 32'h11223344;
  bit [3:0][7:0] proc_reshape;
  bit [7:0] proc_b3, proc_b0;

  initial begin
    init_b3 = init_reshape[3];
    init_b2 = init_reshape[2];
    init_b1 = init_reshape[1];
    init_b0 = init_reshape[0];

    proc_reshape = flat;
    proc_b3 = proc_reshape[3];
    proc_b0 = proc_reshape[0];
  end
endmodule
