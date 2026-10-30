// An assignment operator takes any variable_lvalue on its left, a
// concatenation included (LRM A.6.2, A.8.5), and is a blocking assignment of
// the operator applied to what the left-hand side held, with every left-hand
// index expression evaluated once (LRM 11.4.1). A concatenation is a packed
// vector of its members' bits (LRM 11.4.12), so the operator sees the members
// joined, first member most significant, and its result is split among them
// the same way: a carry out of one member lands in the member above it. The
// increment and decrement operators are blocking assignments of the same kind
// (LRM 11.4.2), and where one is read as a value the postfix form yields what
// the members held before the step.
module Top;
  logic [3:0] and_high, and_low;
  logic [3:0] add_high, add_low;
  logic [3:0] shift_high, shift_low;
  logic [3:0] nested_a, nested_b, nested_c;
  logic [3:0] inc_high, inc_low;
  logic [3:0] dec_high, dec_low;
  logic [3:0] post_high, post_low;
  logic [7:0] post_value;
  logic [3:0] pre_high, pre_low;
  logic [7:0] pre_value;

  logic [3:0] slots[4];
  logic [3:0] beside;
  int calls;

  function automatic int next_slot();
    calls++;
    return calls;
  endfunction

  initial begin
    {and_high, and_low} = 8'hFF;
    {and_high, and_low} &= 8'h3C;

    {add_high, add_low} = 8'h2F;
    {add_high, add_low} += 8'h01;

    {shift_high, shift_low} = 8'h0B;
    {shift_high, shift_low} <<= 2;

    {nested_a, {nested_b, nested_c}} = 12'hFFF;
    {nested_a, {nested_b, nested_c}} ^= 12'h5A3;

    {inc_high, inc_low} = 8'h7F;
    {inc_high, inc_low}++;

    {dec_high, dec_low} = 8'h50;
    --{dec_high, dec_low};

    {post_high, post_low} = 8'h1F;
    post_value = 8'h00;
    post_value = {post_high, post_low}++;

    {pre_high, pre_low} = 8'h1F;
    pre_value = 8'h00;
    pre_value = ++{pre_high, pre_low};

    slots[0] = 4'h0;
    slots[1] = 4'h6;
    slots[2] = 4'h0;
    slots[3] = 4'h0;
    beside = 4'hF;
    calls = 0;
    {slots[next_slot()], beside} += 8'h01;
  end

  final begin
    if ({and_high, and_low} !== 8'h3C)
      $fatal(1, "&= left %h, expected 3c", {and_high, and_low});
    if (add_high !== 4'h3) $fatal(1, "add_high was %h, expected 3", add_high);
    if (add_low !== 4'h0) $fatal(1, "add_low was %h, expected 0", add_low);
    if ({shift_high, shift_low} !== 8'h2C)
      $fatal(1, "<<= left %h, expected 2c", {shift_high, shift_low});
    if (nested_a !== 4'hA) $fatal(1, "nested_a was %h, expected a", nested_a);
    if (nested_b !== 4'h5) $fatal(1, "nested_b was %h, expected 5", nested_b);
    if (nested_c !== 4'hC) $fatal(1, "nested_c was %h, expected c", nested_c);

    if ({inc_high, inc_low} !== 8'h80)
      $fatal(1, "++ left %h, expected 80", {inc_high, inc_low});
    if ({dec_high, dec_low} !== 8'h4F)
      $fatal(1, "-- left %h, expected 4f", {dec_high, dec_low});
    if (post_value !== 8'h1F)
      $fatal(1, "postfix yielded %h, expected 1f", post_value);
    if ({post_high, post_low} !== 8'h20)
      $fatal(1, "postfix left %h, expected 20", {post_high, post_low});
    if (pre_value !== 8'h20)
      $fatal(1, "prefix yielded %h, expected 20", pre_value);
    if ({pre_high, pre_low} !== 8'h20)
      $fatal(1, "prefix left %h, expected 20", {pre_high, pre_low});

    if (calls !== 1) $fatal(1, "the index was evaluated %0d times", calls);
    if (slots[1] !== 4'h7) $fatal(1, "slots[1] was %h, expected 7", slots[1]);
    if (beside !== 4'h0) $fatal(1, "beside was %h, expected 0", beside);
    if (slots[2] !== 4'h0) $fatal(1, "slots[2] was %h, expected 0", slots[2]);
    $display("All checks passed");
  end
endmodule
