// A net-typed input port (LRM 23.3.3): the parent drives the child's input
// `wire` across the port boundary. The child's input net owns its own resolved
// cell and resolves its single driver -- the connection the parent attaches --
// so the child reads the parent's value. The driver attaches at Resolve, seeds
// at Initialize, and the parent's process updates it when the source changes.
module Child(input wire [7:0] in_net);
  initial begin
    #1;
    $display("child sees %0d", in_net);
  end
endmodule

module Top;
  logic [7:0] src;
  Child u(.in_net(src));
  initial src = 8'd42;
endmodule
