// An expression takes any number of operands however wide each is (LRM 11.2;
// LRM 6.9.1 lets a vector be at least 65,536 bits): a chain of three hundred
// operators over values twenty thousand bits wide evaluates like its short
// form wherever it stands -- assigned, by nonblocking assignment, as what a
// function returns, passed to a function and to a task, in a variable's
// initializer, as a loop's condition, evaluated afresh each time round, and in
// an arm of a conditional, evaluated only when that arm is chosen (LRM
// 11.4.11). A structure as wide is converted to its bits the same way (LRM
// 6.24.3).
`define T10(x) x x x x x x x x x x
`define T300(x) `T10(`T10(x)) `T10(`T10(x)) `T10(`T10(x))
`define MEMBER(n) logic [63:0] m``n;
`define MEMBERS(p) `MEMBER(p``0) `MEMBER(p``1) `MEMBER(p``2) `MEMBER(p``3) \
  `MEMBER(p``4) `MEMBER(p``5) `MEMBER(p``6) `MEMBER(p``7) `MEMBER(p``8) \
  `MEMBER(p``9)
module Top;
  typedef logic [19999:0] wide_t;
  typedef struct {
    `MEMBERS(0) `MEMBERS(1) `MEMBERS(2) `MEMBERS(3) `MEMBERS(4)
    `MEMBERS(5) `MEMBERS(6) `MEMBERS(7) `MEMBERS(8) `MEMBERS(9)
    `MEMBERS(10) `MEMBERS(11) `MEMBERS(12) `MEMBERS(13) `MEMBERS(14)
    `MEMBERS(15) `MEMBERS(16) `MEMBERS(17) `MEMBERS(18) `MEMBERS(19)
    `MEMBERS(20) `MEMBERS(21) `MEMBERS(22) `MEMBERS(23) `MEMBERS(24)
    `MEMBERS(25) `MEMBERS(26) `MEMBERS(27) `MEMBERS(28) `MEMBERS(29)
  } wide_s;
  typedef logic [19199:0] wide_s_bits;

  int hits;

  function automatic wide_t one();
    return 1;
  endfunction

  function automatic wide_t counted_one();
    hits = hits + 1;
    return 1;
  endfunction

  function automatic wide_t sum_of(wide_t x);
    return x `T300(+ x);
  endfunction

  function automatic int low_bits(wide_t x);
    return int'(x[31:0]);
  endfunction

  int passed_to_task;
  task automatic take_later(wide_t x);
    #1;
    passed_to_task = int'(x[31:0]);
  endtask

  wide_t a;
  wide_t assigned, deferred, returned, initialized_from = one() `T300(+ one());
  int passed_to_function;
  int rounds;
  wide_t w;
  wide_t not_taken, taken;
  bit take;
  wide_s st;
  wide_s_bits st_bits;

  initial begin
    a = 1;
    assigned = '0;
    deferred = '0;
    returned = '0;
    passed_to_function = -1;
    passed_to_task = -1;
    rounds = -1;
    not_taken = '1;
    taken = '0;
    st_bits = '0;

    assigned = a `T300(+ a);
    deferred <= a `T300(+ a);
    returned = sum_of(a);
    passed_to_function = low_bits(a `T300(+ a));
    take_later(a `T300(+ a));

    w = 0;
    rounds = 0;
    while ((w `T300(+ w)) < 903) begin
      w = w + 1;
      rounds = rounds + 1;
    end

    hits = 0;
    take = 0;
    not_taken = take ? counted_one() `T300(+ counted_one()) : '0;
    if (hits !== 0) $fatal(1, "the arm not taken ran %0d calls", hits);
    take = 1;
    taken = take ? counted_one() `T300(+ counted_one()) : '0;
    if (hits !== 301) $fatal(1, "the arm taken ran %0d calls, expected 301", hits);

    st = '{default: 64'h0123_4567_89ab_cdef};
    st.m00 = 64'hffff_0000_ffff_0000;
    st.m299 = 64'h0000_ffff_0000_ffff;
    st_bits = wide_s_bits'(st);
  end

  final begin
    if (assigned !== 301) $fatal(1, "the assigned chain was %0d, expected 301", assigned);
    if (deferred !== 301) $fatal(1, "the nonblocking chain was %0d, expected 301", deferred);
    if (returned !== 301) $fatal(1, "the returned chain was %0d, expected 301", returned);
    if (passed_to_function !== 301)
      $fatal(1, "the function was passed %0d, expected 301", passed_to_function);
    if (passed_to_task !== 301)
      $fatal(1, "the task was passed %0d, expected 301", passed_to_task);
    if (initialized_from !== 301)
      $fatal(1, "the initializer was %0d, expected 301", initialized_from);
    if (rounds !== 3) $fatal(1, "the loop ran %0d times, expected 3", rounds);
    if (not_taken !== '0) $fatal(1, "the conditional chose the arm not taken");
    if (taken !== 301) $fatal(1, "the arm taken was %0d, expected 301", taken);
    if (st_bits[19199:19136] !== 64'hffff_0000_ffff_0000)
      $fatal(1, "the first member's bits were %h", st_bits[19199:19136]);
    if (st_bits[63:0] !== 64'h0000_ffff_0000_ffff)
      $fatal(1, "the last member's bits were %h", st_bits[63:0]);
    if (st_bits[19135:19072] !== 64'h0123_4567_89ab_cdef)
      $fatal(1, "the second member's bits were %h", st_bits[19135:19072]);
    $display("All checks passed");
  end
endmodule
