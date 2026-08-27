// The map method returns an unpacked array with the same dimension range and
// index type as the source, each element replaced by the value its mandatory
// with expression takes for that element. Each element of the result is typed
// by the self-determined type of that expression rather than by the source's
// element type, so a mapping may widen, narrow, or change the element type
// entirely. An element that is itself an array is an ordinary operand of the
// expression, and a source with no elements maps to a result with no elements
// (LRM 7.12.5).
module Top;
  int base [] = '{1, 2, 3};
  int other [] = '{10, 20, 30};
  bit [7:0] narrow [] = '{8'd5, 8'd6};
  int fixed_source [3] = '{7, 8, 9};
  int queue_source [$] = '{4, 5};
  int empty_source [];
  int rows [][] = '{'{1, 2, 3}, '{4, 5}};
  string words [] = '{"a", "bb"};

  int combined [];
  bit [31:0] widened [];
  bit flags [];
  int from_queue [$];
  int same_range [3];
  int from_empty [];
  int row_totals [];
  string shouted [];

  initial begin
    flags = '{1'b1, 1'b1, 1'b1};
    from_empty = new [3];

    combined = base.map(x) with (x + other[x.index]);
    widened = narrow.map(x) with (x * 300);
    flags = base.map(x) with (x > 1);
    from_queue = queue_source.map(x) with (x * 2);
    same_range = fixed_source.map(x) with (x + 1);
    from_empty = empty_source.map(x) with (x + 1);
    row_totals = rows.map(row) with (row.sum());
    shouted = words.map(s) with (s.toupper());
  end

  final begin
    if (combined.size() !== 3)
      $fatal(1, "combined held %0d elements, expected 3", combined.size());
    if (combined[0] !== 11)
      $fatal(1, "combined[0] was %0d, expected 11", combined[0]);
    if (combined[1] !== 22)
      $fatal(1, "combined[1] was %0d, expected 22", combined[1]);
    if (combined[2] !== 33)
      $fatal(1, "combined[2] was %0d, expected 33", combined[2]);

    if (widened[0] !== 32'd1500)
      $fatal(1, "a widening map expression gave %0d, expected 1500",
             widened[0]);
    if (widened[1] !== 32'd1800)
      $fatal(1, "widened[1] was %0d, expected 1800", widened[1]);

    if (flags.size() !== 3)
      $fatal(1, "flags held %0d elements, expected 3", flags.size());
    if (flags[0] !== 1'b0)
      $fatal(1, "flags[0] was %b, expected 0", flags[0]);
    if (flags[1] !== 1'b1)
      $fatal(1, "flags[1] was %b, expected 1", flags[1]);
    if (flags[2] !== 1'b1)
      $fatal(1, "flags[2] was %b, expected 1", flags[2]);

    if (from_queue.size() !== 2)
      $fatal(1, "from_queue held %0d elements, expected 2", from_queue.size());
    if (from_queue[0] !== 8)
      $fatal(1, "from_queue[0] was %0d, expected 8", from_queue[0]);
    if (from_queue[1] !== 10)
      $fatal(1, "from_queue[1] was %0d, expected 10", from_queue[1]);

    if (same_range[0] !== 8)
      $fatal(1, "same_range[0] was %0d, expected 8", same_range[0]);
    if (same_range[2] !== 10)
      $fatal(1, "same_range[2] was %0d, expected 10", same_range[2]);

    if (from_empty.size() !== 0)
      $fatal(1, "mapping an empty array gave %0d elements, expected 0",
             from_empty.size());

    if (row_totals.size() !== 2)
      $fatal(1, "row_totals held %0d elements, expected 2", row_totals.size());
    if (row_totals[0] !== 6)
      $fatal(1, "row_totals[0] was %0d, expected 6", row_totals[0]);
    if (row_totals[1] !== 9)
      $fatal(1, "row_totals[1] was %0d, expected 9", row_totals[1]);

    if (shouted.size() !== 2)
      $fatal(1, "shouted held %0d elements, expected 2", shouted.size());
    if (shouted[0] != "A")
      $fatal(1, "shouted[0] was '%s', expected 'A'", shouted[0]);
    if (shouted[1] != "BB")
      $fatal(1, "shouted[1] was '%s', expected 'BB'", shouted[1]);
    $display("All checks passed");
  end
endmodule
