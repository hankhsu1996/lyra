// An unpacked array or structure passed by reference is the caller's own
// variable inside the subroutine, so reading one element of it, a member of
// it, or bits of an element reads the caller's value, and writing one element
// writes the caller's and leaves the others as they were (LRM 13.5.2). A
// `const ref` formal is read the same way and only forbids the write.
module Top;
  typedef struct {
    int count;
    logic [7:0] code;
  } record_t;

  int row [4];
  int grid [2][2];
  record_t record;
  logic [15:0] words [2];

  int second_seen = -1;
  int total_seen = -1;
  int corner_seen = -1;
  logic [7:0] code_seen = 8'h00;
  logic [3:0] nibble_seen = 4'h0;

  function automatic int second(const ref int xs [4]);
    return xs[1];
  endfunction

  function automatic int total(const ref int xs [4]);
    int sum = 0;
    foreach (xs[i]) sum += xs[i];
    return sum;
  endfunction

  function automatic void set_third(ref int xs [4], input int value);
    xs[2] = value;
  endfunction

  function automatic int carry_over(ref int g [2][2]);
    g[1][0] = g[0][1] + 5;
    return g[1][1];
  endfunction

  function automatic logic [7:0] code_of(const ref record_t r);
    return r.code;
  endfunction

  function automatic logic [3:0] nibble_of(const ref logic [15:0] w [2]);
    return w[1][7:4];
  endfunction

  initial begin
    row = '{1, 2, 3, 4};
    grid = '{'{10, 20}, '{30, 40}};
    record = '{count: 7, code: 8'h5A};
    words = '{16'h1234, 16'hABCD};

    second_seen = second(row);
    total_seen = total(row);
    set_third(row, 30);
    corner_seen = carry_over(grid);
    code_seen = code_of(record);
    nibble_seen = nibble_of(words);
  end

  final begin
    if (second_seen !== 2)
      $fatal(1, "second_seen was %0d, expected 2", second_seen);
    if (total_seen !== 10)
      $fatal(1, "total_seen was %0d, expected 10", total_seen);
    if (row[2] !== 30) $fatal(1, "row[2] was %0d, expected 30", row[2]);
    if (row[1] !== 2) $fatal(1, "row[1] was %0d, expected 2", row[1]);
    if (row[3] !== 4) $fatal(1, "row[3] was %0d, expected 4", row[3]);
    if (corner_seen !== 40)
      $fatal(1, "corner_seen was %0d, expected 40", corner_seen);
    if (grid[1][0] !== 25)
      $fatal(1, "grid[1][0] was %0d, expected 25", grid[1][0]);
    if (grid[0][1] !== 20)
      $fatal(1, "grid[0][1] was %0d, expected 20", grid[0][1]);
    if (code_seen !== 8'h5A)
      $fatal(1, "code_seen was %h, expected 5a", code_seen);
    if (nibble_seen !== 4'hC)
      $fatal(1, "nibble_seen was %h, expected c", nibble_seen);
    $display("All checks passed");
  end
endmodule
