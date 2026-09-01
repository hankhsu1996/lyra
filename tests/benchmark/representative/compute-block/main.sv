// @measure: run
// @work: table pass
//
// One pass walks a table of operands, dispatches on three bits of each through
// a case statement, and writes the result back. It mixes arithmetic, control
// flow and data access deliberately: the cases beside it each isolate one cost
// family, and this one is here to catch a regression that only shows when they
// meet.
module Top;
  localparam int TABLE_SIZE = 1024;

  logic [31:0] operands [0:TABLE_SIZE-1];

  initial begin
    int num_passes;
    logic [31:0] acc;
    logic [31:0] val;
    int opcode;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    for (int i = 0; i < TABLE_SIZE; i++)
      operands[i] = 32'(i * 2654435);

    acc = 0;

    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < TABLE_SIZE; i++) begin
        val = operands[i];
        opcode = int'(val[2:0]);

        case (opcode)
          0: acc = acc + val;
          1: acc = acc - val;
          2: acc = acc ^ val;
          3: acc = acc | (val >> 1);
          4: acc = acc & ~val;
          5: acc = (acc < val) ? acc + 1 : acc - 1;
          6: acc = acc + (val << 2);
          7: acc = acc ^ {val[15:0], val[31:16]};
          default: acc = acc + 1;
        endcase

        operands[i] = val ^ acc;
      end
    end

    $display("compute-block done: acc=%0d", acc);
    $finish;
  end
endmodule
