// Setting the initial value of a static variable as part of its declaration
// happens before any initial or always procedure starts, and the standard says
// this of static class members in the same breath as of any other static
// variable. So a procedure reads the declared value however early it runs, a
// continuous assignment carries it at time zero, and the initialization is not
// repeated when objects of the class are constructed -- the cell is created and
// initialized once whatever else the class does. A class a package declares
// owns one such cell for the whole program, reached by the class scope
// resolution operator from any scope (LRM 10.5, 8.9, 26.2).
package count_pkg;
  class Tally;
    static int started = 100;
    static int made = 0;
    static logic [3:0] mask;

    function new();
      made = made + 1;
    endfunction

    static function int started_plus(int extra);
      return started + extra;
    endfunction
  endclass
endpackage

module Top;
  int seen_in_initial;
  int seen_continuously;
  int made_before_any;
  int made_after_two;
  int started_after_two;
  int started_through_method;
  logic [3:0] mask_before_any;

  assign seen_continuously = count_pkg::Tally::started;

  initial begin
    count_pkg::Tally first;
    count_pkg::Tally second;

    made_before_any = count_pkg::Tally::made;
    mask_before_any = count_pkg::Tally::mask;
    seen_in_initial = count_pkg::Tally::started;

    first = new;
    second = new;

    made_after_two = count_pkg::Tally::made;
    started_after_two = count_pkg::Tally::started;
    started_through_method = count_pkg::Tally::started_plus(5);
  end

  final begin
    // The declared value is in place before this procedure could have run.
    if (seen_in_initial !== 100)
      $fatal(1, "seen_in_initial was %0d, expected 100", seen_in_initial);
    if (seen_continuously !== 100)
      $fatal(1, "seen_continuously was %0d, expected 100", seen_continuously);
    // A static property declared without a value takes its type default, and
    // that default is in place before the first constructor reads it. A
    // four-state one takes x, which no write in this design could produce.
    if (made_before_any !== 0)
      $fatal(1, "made_before_any was %0d, expected 0", made_before_any);
    if (mask_before_any !== 4'bxxxx)
      $fatal(1, "mask_before_any was %b, expected xxxx", mask_before_any);
    if (made_after_two !== 2)
      $fatal(1, "made_after_two was %0d, expected 2", made_after_two);
    // Constructing objects does not re-run the declaration's initializer.
    if (started_after_two !== 100)
      $fatal(1, "started_after_two was %0d, expected 100", started_after_two);
    if (started_through_method !== 105)
      $fatal(1, "started_through_method was %0d, expected 105",
             started_through_method);
    $display("All checks passed");
  end
endmodule
