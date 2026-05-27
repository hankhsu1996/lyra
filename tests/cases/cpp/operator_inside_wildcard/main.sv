module Top;
  logic item_z_wildcard_match;
  logic item_z_wildcard_no_match;
  logic item_x_wildcard_match;
  logic item_question_wildcard_match;
  logic mixed_value_and_wildcard_item;
  initial begin
    logic [3:0] v;

    v = 4'b1010;
    item_z_wildcard_match = (v inside {4'b10zz}) ? 1 : 0;

    v = 4'b0010;
    item_z_wildcard_no_match = (v inside {4'b10zz}) ? 1 : 0;

    v = 4'b1010;
    item_x_wildcard_match = (v inside {4'b10xx}) ? 1 : 0;

    v = 4'b1011;
    item_question_wildcard_match = (v inside {4'b10??}) ? 1 : 0;

    v = 4'b1010;
    mixed_value_and_wildcard_item = (v inside {4'd0, 4'b10zz, 4'd15}) ? 1 : 0;
  end
endmodule
