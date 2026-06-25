module Top;
  logic [3:0] x;
  logic [7:0] s_ext;
  logic [7:0] u_ext;

  // $signed reinterprets the operand as signed, so widening to 8 bits
  // sign-extends (4'b1111 -> -1 -> 8'hFF); $unsigned zero-extends (-> 8'h0F).
  assign x = 4'b1111;
  assign s_ext = $signed(x);
  assign u_ext = $unsigned(x);
endmodule
