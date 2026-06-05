module Top;
  int empty [];
  int single [];
  int multi [];
  int matrix [][];
  initial begin
    // Empty array reverses to empty (no-op).
    empty.reverse();
    // Single element is a no-op too.
    single = new[1];
    single[0] = 42;
    single.reverse();
    // Multi-element reversal restores the input in reverse order.
    multi = new[5];
    multi[0] = 10;
    multi[1] = 20;
    multi[2] = 30;
    multi[3] = 40;
    multi[4] = 50;
    multi.reverse();
    // Multi-dim outer-dim reversal swaps rows; inner rows are preserved
    // intact per row (no nested flip).
    matrix = new[2];
    matrix[0] = new[3];
    matrix[1] = new[3];
    matrix[0][0] = 1;  matrix[0][1] = 2;  matrix[0][2] = 3;
    matrix[1][0] = 10; matrix[1][1] = 20; matrix[1][2] = 30;
    matrix.reverse();
  end
endmodule
