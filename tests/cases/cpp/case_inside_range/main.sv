module Top;
  int val;
  int low;
  int mid;
  int high;
  int oor;

  initial begin
    low = 0; mid = 0; high = 0; oor = 0;

    val = 2;
    case (val) inside
      [1:3]: low = 1;
      [4:6]: mid = 1;
      [7:9]: high = 1;
    endcase

    val = 5;
    case (val) inside
      [1:3]: low = 2;
      [4:6]: mid = 2;
      [7:9]: high = 2;
    endcase

    val = 8;
    case (val) inside
      [1:3]: low = 3;
      [4:6]: mid = 3;
      [7:9]: high = 3;
    endcase

    val = 100;
    case (val) inside
      [1:3]: low = 9;
      [4:6]: mid = 9;
      [7:9]: high = 9;
      default: oor = 1;
    endcase
  end
endmodule
