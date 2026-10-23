// An if-else property evaluates its condition where the attempt starts and
// then is the property that condition selects; with no else, a false condition
// makes it true (LRM 16.12.6). A condition that is x or z is not true, so it
// selects the else property. A case property compares its expression with the
// items' expressions in order, as a case statement does (LRM 12.5), evaluates
// the property of the first item that matches, and otherwise the default's;
// with no item matching and no default it is true (LRM 16.12.16). The
// comparison is a case statement's: every operand is extended to the widest of
// them, unsigned unless all are signed, and an item may be any expression,
// read at the tick the attempt starts.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  logic c = 0;
  logic d = 0;
  logic [1:0] sel = 2'b00;

  always #5 clk = ~clk;

  // Ticks k = 0..5 land at 10k + 5, and each value is written at 10k, so the
  // tick reads it sampled. The run ends at 60, before a seventh tick.
  localparam logic [0:5] A = 6'b101101;
  localparam logic [0:5] B = 6'b000100;
  localparam logic [0:5] C = 6'b110100;
  localparam logic [0:5] D = 6'b010010;
  localparam logic [1:0] S[6] = '{2'd0, 2'd1, 2'd2, 2'bxx, 2'd3, 2'd1};

  initial begin
    for (int k = 0; k < 6; k++) begin
      a = A[k];
      b = B[k];
      c = C[k];
      d = D[k];
      sel = S[k];
      #10;
    end
  end

  initial #60 $finish;

  int both_pass = 0, both_fail = 0;
  int alone_pass = 0, alone_fail = 0;
  int unknown_pass = 0, unknown_fail = 0;
  int items_pass = 0, items_fail = 0;
  int default_pass = 0, default_fail = 0;
  int first_pass = 0, first_fail = 0;
  int widths_pass = 0, widths_fail = 0;

  // `a` is read at the tick the attempt starts, and the arm it selects is
  // read from there on.
  a_both: assert property (@(posedge clk) if (a) (c ##1 d) else (b ##1 1'b1))
      both_pass++;
    else
      both_fail++;
  a_alone: assert property (@(posedge clk) if (c) (d ##1 a))
      alone_pass++;
    else
      alone_fail++;
  a_unknown: assert property (@(posedge clk) if (sel[0]) b else a)
      unknown_pass++;
    else
      unknown_fail++;
  a_items: assert property (@(posedge clk)
      case (sel)
        2'd0: (c ##1 d);
        2'd1, 2'd2: d;
        2'bxx: 1'b1;
      endcase)
      items_pass++;
    else
      items_fail++;
  a_default: assert property (@(posedge clk)
      case (sel)
        2'd1: a;
        default: b;
      endcase)
      default_pass++;
    else
      default_fail++;
  // Where `c` is high, the first item's label is 1 as well as the second's,
  // and the first one written is the one taken.
  a_first: assert property (@(posedge clk)
      case (sel)
        {1'b0, c}: a;
        2'd1: 1'b1;
        default: 1'b1;
      endcase)
      first_pass++;
    else
      first_fail++;
  // `sel` is compared as a 32-bit unsigned value: 3'b001 matches where it is
  // 1, and -1 is 32 ones, which no value of `sel` reaches; read as two bits it
  // would be 3, taken at k = 4 where `b` is low.
  a_widths: assert property (@(posedge clk)
      case (sel)
        3'b001: a;
        -1: b;
        default: 1'b1;
      endcase)
      widths_pass++;
    else
      widths_fail++;

  function automatic void check(string name, int passes, int fails,
                                int want_passes, int want_fails);
    if (passes !== want_passes || fails !== want_fails)
      $fatal(1, "%s held %0d and failed %0d times, expected %0d and %0d",
             name, passes, fails, want_passes, want_fails);
  endfunction

  final begin
    check("if-else", both_pass, both_fail, 2, 4);
    // `c` is false at k = 2, 4 and 5, where the property is true without
    // `d ##1 a` being read.
    check("if without else", alone_pass, alone_fail, 4, 2);
    // At k = 3 `sel[0]` is x and selects `a`.
    check("if on an unknown condition", unknown_pass, unknown_fail, 3, 3);
    // At k = 3 only the item written 2'bxx matches, and at k = 4 nothing
    // does and there is no default.
    check("case items", items_pass, items_fail, 4, 2);
    // At k = 3 no item matches x, so the default is taken.
    check("case default", default_pass, default_fail, 2, 4);
    // Only k = 1 takes the first item, where `a` is low; at k = 5 `c` is low
    // and the second item is taken.
    check("case first item", first_pass, first_fail, 5, 1);
    // Only k = 1 and k = 5 match 3'b001; `a` is low at the first.
    check("case widths", widths_pass, widths_fail, 5, 1);
    $display("All checks passed");
  end
endmodule
