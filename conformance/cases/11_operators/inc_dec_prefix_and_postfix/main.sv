// The increment and decrement operators add or subtract one from their
// operand and assign the result back to it. The prefix forms ++i and --i
// yield the updated value and the postfix forms i++ and i-- yield the value
// the operand held before the update. They need no parentheses in an
// expression, and used as a statement they leave only the update behind
// (LRM 11.4.2).
module Top;
  int post_inc_operand;
  int post_inc_value;
  int post_dec_operand;
  int post_dec_value;
  int pre_inc_operand;
  int pre_inc_value;
  int pre_dec_operand;
  int pre_dec_value;
  int stmt_post_inc;
  int stmt_pre_inc;
  int stmt_post_dec;
  int stmt_pre_dec;
  int expr_left;
  int expr_right;
  int expr_sum;
  int expr_left_after;
  int expr_right_after;

  initial begin
    post_inc_operand = 5;
    post_inc_value = post_inc_operand++;

    post_dec_operand = 5;
    post_dec_value = post_dec_operand--;

    pre_inc_operand = 5;
    pre_inc_value = ++pre_inc_operand;

    pre_dec_operand = 5;
    pre_dec_value = --pre_dec_operand;

    stmt_post_inc = 10;
    stmt_post_inc++;

    stmt_pre_inc = 10;
    ++stmt_pre_inc;

    stmt_post_dec = 10;
    stmt_post_dec--;

    stmt_pre_dec = 10;
    --stmt_pre_dec;

    expr_left = 5;
    expr_right = 10;
    expr_sum = ++expr_left + expr_right--;
    expr_left_after = expr_left;
    expr_right_after = expr_right;
  end

  final begin
    if (post_inc_value !== 5)
      $fatal(1, "i++ yielded %0d, expected 5", post_inc_value);
    if (post_inc_operand !== 6)
      $fatal(1, "i++ left the operand at %0d, expected 6", post_inc_operand);
    if (post_dec_value !== 5)
      $fatal(1, "i-- yielded %0d, expected 5", post_dec_value);
    if (post_dec_operand !== 4)
      $fatal(1, "i-- left the operand at %0d, expected 4", post_dec_operand);
    if (pre_inc_value !== 6)
      $fatal(1, "++i yielded %0d, expected 6", pre_inc_value);
    if (pre_inc_operand !== 6)
      $fatal(1, "++i left the operand at %0d, expected 6", pre_inc_operand);
    if (pre_dec_value !== 4)
      $fatal(1, "--i yielded %0d, expected 4", pre_dec_value);
    if (pre_dec_operand !== 4)
      $fatal(1, "--i left the operand at %0d, expected 4", pre_dec_operand);
    if (stmt_post_inc !== 11)
      $fatal(1, "i++ as a statement gave %0d, expected 11", stmt_post_inc);
    if (stmt_pre_inc !== 11)
      $fatal(1, "++i as a statement gave %0d, expected 11", stmt_pre_inc);
    if (stmt_post_dec !== 9)
      $fatal(1, "i-- as a statement gave %0d, expected 9", stmt_post_dec);
    if (stmt_pre_dec !== 9)
      $fatal(1, "--i as a statement gave %0d, expected 9", stmt_pre_dec);
    if (expr_sum !== 16)
      $fatal(1, "++a + b-- yielded %0d, expected 16", expr_sum);
    if (expr_left_after !== 6)
      $fatal(1, "++a left a at %0d, expected 6", expr_left_after);
    if (expr_right_after !== 9)
      $fatal(1, "b-- left b at %0d, expected 9", expr_right_after);
    $display("All checks passed");
  end
endmodule
