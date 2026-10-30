// The set membership form makes the case expression the left operand and each
// item expression the right one, so its wildcard matching is asymmetric: an x
// or z in an item stands for any value, while an x or z in the case expression
// stands only for itself (LRM 12.5.4, LRM 11.4.13). An item is matched only
// when the membership operation returns 1'b1, so an item that leaves the
// comparison unknown is passed over for the default (LRM 12.5.4).
module Top;
  logic [3:0] code;
  int wildcard_on_item;
  int x_on_expression;
  int x_under_wildcard;

  initial begin
    code = 4'b1010;
    wildcard_on_item = 0;
    case (code) inside
      4'b00??: wildcard_on_item = 1;
      4'b10??: wildcard_on_item = 2;
      4'b11??: wildcard_on_item = 3;
      default: wildcard_on_item = 99;
    endcase

    code = 4'b101x;
    x_on_expression = 0;
    case (code) inside
      4'b1010: x_on_expression = 1;
      default: x_on_expression = 99;
    endcase

    code = 4'b101x;
    x_under_wildcard = 0;
    case (code) inside
      4'b101?: x_under_wildcard = 1;
      default: x_under_wildcard = 99;
    endcase
  end

  final begin
    if (wildcard_on_item !== 2)
      $fatal(1, "wildcard_on_item was %0d, expected 2", wildcard_on_item);
    if (x_on_expression !== 99)
      $fatal(1, "x_on_expression was %0d, expected 99", x_on_expression);
    if (x_under_wildcard !== 1)
      $fatal(1, "x_under_wildcard was %0d, expected 1", x_under_wildcard);
    $display("All checks passed");
  end
endmodule
