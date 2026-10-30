// A function defined in a module without the automatic keyword is static, and
// a variable declared inside it is static too: one variable exists for the
// whole simulation and keeps its value from one call to the next. An automatic
// function may still declare particular variables static, each block that
// declares one gets its own even where two sibling blocks choose the same
// name, and an initializer on such a declaration runs once at time zero rather
// than on each entry (LRM 6.21, 13.4.2).
module Top;
  int id1;
  int id2;
  int id3;
  int mix1;
  int mix2;
  int sib1;
  int sib2;

  function int next_id();
    int counter;
    counter = counter + 1;
    return counter;
  endfunction

  function automatic int mix();
    int a;
    static int hits;
    hits = hits + 1;
    a = hits * 2;
    return a;
  endfunction

  function automatic int siblings();
    automatic int a;
    automatic int b;
    begin
      static int x = 0;
      x = x + 1;
      a = x;
    end
    begin
      static int x = 0;
      x = x + 10;
      b = x;
    end
    return a * 100 + b;
  endfunction

  initial begin
    id1 = next_id();
    id2 = next_id();
    id3 = next_id();

    mix1 = mix();
    mix2 = mix();

    sib1 = siblings();
    sib2 = siblings();
  end

  final begin
    if (id1 !== 1) $fatal(1, "id1 was %0d, expected 1", id1);
    if (id2 !== 2) $fatal(1, "id2 was %0d, expected 2", id2);
    if (id3 !== 3) $fatal(1, "id3 was %0d, expected 3", id3);
    if (mix1 !== 2) $fatal(1, "mix1 was %0d, expected 2", mix1);
    if (mix2 !== 4) $fatal(1, "mix2 was %0d, expected 4", mix2);
    if (sib1 !== 110) $fatal(1, "sib1 was %0d, expected 110", sib1);
    if (sib2 !== 220) $fatal(1, "sib2 was %0d, expected 220", sib2);
    $display("All checks passed");
  end
endmodule
