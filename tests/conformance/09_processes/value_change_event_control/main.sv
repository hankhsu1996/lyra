// An event control whose expression carries no edge qualifier resumes the
// procedure on any change in the value of that expression, and a write that
// leaves the expression's value as it was is not an event at all (LRM 9.4.2).
// The expression need only reduce to a singular value, so a string, a real or
// an enumeration serves as well as an integral one. When the expression is a
// select, its value is made of the bits the select names, so a change confined
// to the bits outside the select is not a change in the expression, whichever
// direction the dimension is declared in and wherever its bounds start.
typedef enum logic [1:0] {Red, Green, Blue} colour_t;

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
    #5;
    descending = 8'b0010_1000;
    two_dimensional[0] = 8'hFF;
    ascending[5] = 1'b1;
    offset[6] = 1'b1;
    repeated = 1'b0;
    #5;
    changing = 1'b1;
    vector = 8'h42;
    descending[3] = 1'b0;
    two_dimensional[1] = 8'hAA;
    ascending[2] = 1'b1;
    offset[0] = 1'b1;
    repeated = 1'b1;
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
    $display("All checks passed");
  end
endmodule
