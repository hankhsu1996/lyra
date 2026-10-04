// A foreach loop variable is automatic (LRM 12.7.3), and an automatic variable
// a fork-join_none branch refers to stays in existence until that branch has
// finished with it, even though the scope declaring it has been left (LRM
// 6.21, LRM 9.3.2). So a branch that delays past the end of the loop and then
// reads the loop variable still runs, for a fixed dimension, a dynamically
// sized one, and an associative one alike. The standard does not say what
// value the variable holds once the loop has run out of indices, so what is
// held here is that every branch ran and that all of them, reading the one
// variable after the loop is over, read the same value.
//
// A variable declared automatic in the fork's own declarations is initialized
// each time the fork is entered, before any branch starts (LRM 9.3.2), so a
// branch that copies the loop variable that way sees its own iteration's
// index however late it runs.
module Top;
  int fixed [3];
  int fixed_ran;
  int fixed_first;
  int fixed_differ = -1;
  int fixed_copies;

  int dyn [];
  int dyn_ran;
  int dyn_first;
  int dyn_differ = -1;
  int dyn_copies;

  int by_key [string];
  int key_ran;
  string key_first;
  int key_differ = -1;
  int key_copied_a;
  int key_copied_b;

  initial begin
    fixed_differ = 0;
    foreach (fixed[i]) begin
      fork
        automatic int at = i;
        begin
          #1;
          if (fixed_ran == 0) fixed_first = i;
          else if (i != fixed_first) fixed_differ = fixed_differ + 1;
          fixed_ran = fixed_ran + 1;
          fixed_copies = fixed_copies | (1 << at);
        end
      join_none
    end

    dyn = new[2];
    dyn_differ = 0;
    foreach (dyn[i]) begin
      fork
        automatic int at = i;
        begin
          #1;
          if (dyn_ran == 0) dyn_first = i;
          else if (i != dyn_first) dyn_differ = dyn_differ + 1;
          dyn_ran = dyn_ran + 1;
          dyn_copies = dyn_copies | (1 << at);
        end
      join_none
    end

    by_key["a"] = 1;
    by_key["b"] = 2;
    key_differ = 0;
    foreach (by_key[k]) begin
      fork
        automatic string at = k;
        begin
          #1;
          if (key_ran == 0) key_first = k;
          else if (k != key_first) key_differ = key_differ + 1;
          key_ran = key_ran + 1;
          if (at == "a") key_copied_a = key_copied_a + 1;
          if (at == "b") key_copied_b = key_copied_b + 1;
        end
      join_none
    end
  end

  final begin
    if (fixed_ran !== 3) $fatal(1, "fixed_ran was %0d, expected 3", fixed_ran);
    if (fixed_differ !== 0)
      $fatal(1, "fixed_differ was %0d, expected 0", fixed_differ);
    if (fixed_copies !== 7)
      $fatal(1, "fixed_copies was %0d, expected 7", fixed_copies);
    if (dyn_ran !== 2) $fatal(1, "dyn_ran was %0d, expected 2", dyn_ran);
    if (dyn_differ !== 0)
      $fatal(1, "dyn_differ was %0d, expected 0", dyn_differ);
    if (dyn_copies !== 3)
      $fatal(1, "dyn_copies was %0d, expected 3", dyn_copies);
    if (key_ran !== 2) $fatal(1, "key_ran was %0d, expected 2", key_ran);
    if (key_differ !== 0)
      $fatal(1, "key_differ was %0d, expected 0", key_differ);
    if (key_copied_a !== 1)
      $fatal(1, "key_copied_a was %0d, expected 1", key_copied_a);
    if (key_copied_b !== 1)
      $fatal(1, "key_copied_b was %0d, expected 1", key_copied_b);
    $display("All checks passed");
  end
endmodule
