// One module instantiated once per block of a generate loop, each instance
// handed a different value that only says what the instance builds: which
// alternative of a conditional generate stands (LRM 27.5), which item of a
// case generate, and how many blocks a loop generate counts out (LRM 27.4).
// N is how many instances there are.
//
// Which blocks an instance built is part of its unit's name, so the child is
// one unit per set of blocks its values build -- at most eight here, whatever
// N is -- and the loop's block, named by what it declares, is one class per
// unit of the child it holds. Each instance is handed a value of its own, so
// each is a specialization of its own, lowered to HIR on its own and built
// from a table entry of its own; that is what each step adds.
// @each-adds: bytes, bodies lowered 1
module Leaf #(parameter int K = 0) (input var bit c, output int o);
  localparam bit ODD = K % 2 != 0;
  int stages [4];
  if (ODD) begin : parity
    always @(posedge c) o <= 1;
  end else begin : parity
    always @(posedge c) o <= 2;
  end
  case (K % 3)
    0: begin : third
      initial stages[3] = 10;
    end
    default: begin : third
      initial stages[3] = 20;
    end
  endcase
  for (genvar s = 0; s < K % 4; s += 1) begin : stage
    initial stages[s] = s;
  end
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int o [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    Leaf #(.K(i)) u (.c(clk), .o(o[i]));
  end
endmodule
