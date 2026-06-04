module Top;
  int matrix [][];
  initial begin
    matrix = new[2];
    matrix[0] = new[3];
    matrix[1] = new[3];
    matrix[0][0] = 1; matrix[0][1] = 2; matrix[0][2] = 3;
    matrix[1][0] = 10; matrix[1][1] = 20; matrix[1][2] = 30;
  end
endmodule
