// A call to a function that has output or inout arguments is illegal only in
// an event expression, in an expression within a procedural continuous
// assignment, and in an expression that is not within a procedural statement.
// Within a procedural statement such a call is an ordinary operand: it yields
// its return value into the surrounding expression -- an arithmetic operand,
// the argument of another call, the condition of an if -- and the copy back
// to its actual still happens (LRM 13.4, 13.5). A string or a queue returned
// that way is the surrounding expression's to read, as an operand of a
// concatenation, the object a method is called on, or another call's
// argument, call after call.
module Top;
  typedef int int_q[$];

  int arith_result;
  int made_arith;
  int nested_result;
  int made_nested;
  int cond_result;
  int made_cond;
  int inout_result;
  int bumped;
  string joined;
  int made_name;
  int lengths;
  int queue_sizes;
  int made_queue;

  function automatic int make(input int a, output int o);
    o = a * 10;
    return a + 1;
  endfunction

  function automatic int wrap(input int x);
    return x * 2;
  endfunction

  function automatic int bump(inout int v);
    v = v + 1;
    return v * 10;
  endfunction

  // Long enough that a string holds it in storage of its own rather than
  // inside itself, so reading it after its end reads storage given back.
  localparam string prefix = "a name long enough to be kept apart from its holder, number ";

  function automatic string name_of(input int a, output int o);
    string s;
    o = a;
    s = $sformatf("%s%0d", prefix, a);
    return s;
  endfunction

  function automatic int_q items_of(input int n, output int o);
    int_q q;
    o = n;
    for (int i = 0; i < n; i++) q.push_back(i);
    return q;
  endfunction

  function automatic int length_of(input string s);
    return s.len();
  endfunction

  initial begin
    for (int i = 0; i < 100; i++) begin
      joined = {name_of(i, made_name), "!"};
      lengths += length_of(name_of(i, made_name));
      queue_sizes += items_of(i % 8, made_queue).size();
    end
    arith_result = make(3, made_arith) + 100;
    nested_result = wrap(make(5, made_nested));
    if (make(7, made_cond) > 0) cond_result = 1;
    else cond_result = 2;
    bumped = 5;
    inout_result = bump(bumped) + 1000;
  end

  final begin
    if (arith_result !== 104)
      $fatal(1, "arith_result was %0d, expected 104", arith_result);
    if (made_arith !== 30)
      $fatal(1, "made_arith was %0d, expected 30", made_arith);
    if (nested_result !== 12)
      $fatal(1, "nested_result was %0d, expected 12", nested_result);
    if (made_nested !== 50)
      $fatal(1, "made_nested was %0d, expected 50", made_nested);
    if (cond_result !== 1)
      $fatal(1, "cond_result was %0d, expected 1", cond_result);
    if (made_cond !== 70)
      $fatal(1, "made_cond was %0d, expected 70", made_cond);
    if (inout_result !== 1060)
      $fatal(1, "inout_result was %0d, expected 1060", inout_result);
    if (bumped !== 6) $fatal(1, "bumped was %0d, expected 6", bumped);
    if (joined != {prefix, "99!"})
      $fatal(1, "joined was %s, expected %s99!", joined, prefix);
    if (made_name !== 99)
      $fatal(1, "made_name was %0d, expected 99", made_name);
    if (lengths !== 6190)
      $fatal(1, "lengths was %0d, expected 6190", lengths);
    if (queue_sizes !== 342)
      $fatal(1, "queue_sizes was %0d, expected 342", queue_sizes);
    if (made_queue !== 3)
      $fatal(1, "made_queue was %0d, expected 3", made_queue);
    $display("All checks passed");
  end
endmodule
