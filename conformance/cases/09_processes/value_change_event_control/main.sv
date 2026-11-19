// An event control whose expression carries no edge qualifier resumes the
// procedure on any change in the value of that expression, and a write that
// leaves the expression's value as it was is not an event at all (LRM 9.4.2).
// The expression need only reduce to a singular value, so a string, a real or
// an enumeration serves as well as an integral one. When the expression is a
// select, its value is made of the bits the select names, so a change confined
// to the bits outside the select is not a change in the expression, whichever
// direction the dimension is declared in and wherever its bounds start. An
// element of an unpacked array and a field of an unpacked struct are the same
// requirement over parts that are not bit ranges: the expression's value is
// that one part, so a write to a sibling part changes an operand of the
// expression and not its result, which is no event. A slice written over
// several elements at once is one assignment (LRM 7.6) and the same holds of
// each element it reaches: a wait on an element the slice changed resumes,
// wherever in the slice that element lies, whatever part of the slice falls
// outside the array (LRM 7.4.5), and whether the slice is of a fixed-size or a
// dynamic array or lies within an element of another array, while a wait on an
// element of the slice that kept its value does not. A queue's slice is the
// elements between two positions, clamped to the queue (LRM 7.10.1), and is
// held to the same rule.
typedef enum logic [1:0] {Red, Green, Blue} colour_t;

typedef struct {
  int watched;
  int sibling;
} pair_t;

module Top;
  logic changing = 1'b0;
  logic repeated = 1'b0;
  logic [7:0] vector = 8'h00;
  string text = "x";
  real number = 0.0;
  colour_t colour = Red;

  logic [7:0] descending = 8'b0000_1000;
  logic [1:0][7:0] two_dimensional = '0;
  logic [0:7] ascending = '0;
  logic [-1:6] offset = '0;

  int mem[0:7];
  pair_t pair;

  int run_fixed[0:7];
  int run_dynamic[] = '{0, 0, 0, 0, 0, 0, 0, 0};
  int run_low[0:3];
  int run_grid[0:1][0:3];
  int run_queue[$] = '{0, 0, 0, 0, 0, 0};
  int low = -2;

  time changing_at;
  int repeated_wakes;
  time repeated_last_at;
  time vector_at;
  time text_at;
  time number_at;
  time colour_at;
  time descending_at;
  time descending_again_at;
  time two_dimensional_at;
  time ascending_at;
  time offset_at;
  time mem_at;
  time pair_at;
  time run_fixed_first_at;
  time run_fixed_kept_at = 0;
  time run_fixed_last_at;
  time run_dynamic_first_at;
  time run_dynamic_kept_at = 0;
  time run_dynamic_last_at;
  time run_low_at;
  time run_grid_kept_at = 0;
  time run_grid_last_at;
  time run_queue_first_at;
  time run_queue_kept_at = 0;
  time run_queue_last_at;

  initial begin
    @(changing);
    changing_at = $time;
  end

  initial begin
    repeat (3) begin
      @(repeated);
      repeated_wakes = repeated_wakes + 1;
      repeated_last_at = $time;
    end
  end

  initial begin
    @(vector);
    vector_at = $time;
  end

  initial begin
    @(text);
    text_at = $time;
  end

  initial begin
    @(number);
    number_at = $time;
  end

  initial begin
    @(colour);
    colour_at = $time;
  end

  initial begin
    @(descending[3]);
    descending_at = $time;
    @(descending[3]);
    descending_again_at = $time;
  end

  initial begin
    @(two_dimensional[1]);
    two_dimensional_at = $time;
  end

  initial begin
    @(ascending[1:2]);
    ascending_at = $time;
  end

  initial begin
    @(offset[0]);
    offset_at = $time;
  end

  initial begin
    @(mem[3]);
    mem_at = $time;
  end

  initial begin
    @(pair.watched);
    pair_at = $time;
  end

  initial begin
    @(run_fixed[2]);
    run_fixed_first_at = $time;
  end

  initial begin
    @(run_fixed[3]);
    run_fixed_kept_at = $time;
  end

  initial begin
    @(run_fixed[4]);
    run_fixed_last_at = $time;
  end

  initial begin
    @(run_dynamic[1]);
    run_dynamic_first_at = $time;
  end

  initial begin
    @(run_dynamic[2]);
    run_dynamic_kept_at = $time;
  end

  initial begin
    @(run_dynamic[3]);
    run_dynamic_last_at = $time;
  end

  initial begin
    @(run_low[0]);
    run_low_at = $time;
  end

  initial begin
    @(run_grid[1][1]);
    run_grid_kept_at = $time;
  end

  initial begin
    @(run_grid[1][3]);
    run_grid_last_at = $time;
  end

  initial begin
    @(run_queue[0]);
    run_queue_first_at = $time;
  end

  initial begin
    @(run_queue[1]);
    run_queue_kept_at = $time;
  end

  initial begin
    @(run_queue[2]);
    run_queue_last_at = $time;
  end

  initial begin
    #5;
    descending = 8'b0010_1000;
    two_dimensional[0] = 8'hFF;
    ascending[5] = 1'b1;
    offset[6] = 1'b1;
    mem[4] = 99;
    pair.sibling = 99;
    repeated = 1'b0;
    #5;
    changing = 1'b1;
    vector = 8'h42;
    descending[3] = 1'b0;
    two_dimensional[1] = 8'hAA;
    ascending[2] = 1'b1;
    offset[0] = 1'b1;
    mem[3] = 7;
    pair.watched = 7;
    repeated = 1'b1;
    run_fixed[2:4] = '{5, 0, 6};
    run_dynamic[1:3] = '{5, 0, 6};
    run_low[low+:3] = '{1, 2, 3};
    run_grid[1][1:3] = '{0, 0, 9};
    run_queue[-2:2] = '{5, 0, 6};
    #5;
    text = "y";
    number = 1.0;
    colour = Blue;
    descending[3] = 1'b1;
    repeated = 1'b1;
    #5;
    repeated = 1'b0;
    #5;
    repeated = 1'b1;
  end

  final begin
    if (changing_at !== 10)
      $fatal(1, "changing_at was %0d, expected 10", changing_at);
    if (repeated_wakes !== 3)
      $fatal(1, "repeated_wakes was %0d, expected 3", repeated_wakes);
    if (repeated_last_at !== 25)
      $fatal(1, "repeated_last_at was %0d, expected 25", repeated_last_at);
    if (vector_at !== 10)
      $fatal(1, "vector_at was %0d, expected 10", vector_at);
    if (text_at !== 15) $fatal(1, "text_at was %0d, expected 15", text_at);
    if (number_at !== 15)
      $fatal(1, "number_at was %0d, expected 15", number_at);
    if (colour_at !== 15)
      $fatal(1, "colour_at was %0d, expected 15", colour_at);
    if (descending_at !== 10)
      $fatal(1, "descending_at was %0d, expected 10", descending_at);
    if (descending_again_at !== 15)
      $fatal(1, "descending_again_at was %0d, expected 15",
             descending_again_at);
    if (two_dimensional_at !== 10)
      $fatal(1, "two_dimensional_at was %0d, expected 10",
             two_dimensional_at);
    if (ascending_at !== 10)
      $fatal(1, "ascending_at was %0d, expected 10", ascending_at);
    if (offset_at !== 10)
      $fatal(1, "offset_at was %0d, expected 10", offset_at);
    if (mem_at !== 10) $fatal(1, "mem_at was %0d, expected 10", mem_at);
    if (pair_at !== 10) $fatal(1, "pair_at was %0d, expected 10", pair_at);
    if (run_fixed_first_at !== 10 || run_fixed_last_at !== 10)
      $fatal(1, "a fixed-size array's slice woke its first and last elements at %0d and %0d, expected 10",
             run_fixed_first_at, run_fixed_last_at);
    if (run_fixed_kept_at !== 0)
      $fatal(1, "a fixed-size array's slice woke an element it kept at %0d",
             run_fixed_kept_at);
    if (run_dynamic_first_at !== 10 || run_dynamic_last_at !== 10)
      $fatal(1, "a dynamic array's slice woke its first and last elements at %0d and %0d, expected 10",
             run_dynamic_first_at, run_dynamic_last_at);
    if (run_dynamic_kept_at !== 0)
      $fatal(1, "a dynamic array's slice woke an element it kept at %0d",
             run_dynamic_kept_at);
    if (run_low_at !== 10)
      $fatal(1, "a slice starting below the array woke its one element at %0d, expected 10",
             run_low_at);
    if (run_grid_last_at !== 10)
      $fatal(1, "a slice within an element woke its last element at %0d, expected 10",
             run_grid_last_at);
    if (run_grid_kept_at !== 0)
      $fatal(1, "a slice within an element woke an element it kept at %0d",
             run_grid_kept_at);
    if (run_queue_first_at !== 10 || run_queue_last_at !== 10)
      $fatal(1, "a queue's slice woke its first and last elements at %0d and %0d, expected 10",
             run_queue_first_at, run_queue_last_at);
    if (run_queue_kept_at !== 0)
      $fatal(1, "a queue's slice woke an element it kept at %0d", run_queue_kept_at);
    $display("All checks passed");
  end
endmodule
