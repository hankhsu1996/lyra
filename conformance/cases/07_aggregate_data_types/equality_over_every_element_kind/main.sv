// Arrays of every kind compare element by element, whatever the elements
// are: reals, strings, class handles, arrays and structures. == and != are
// unknown only where an element's comparison is unknown and none is
// unequal, and === and !== are always known. Two arrays that differ do so
// wherever the difference lies -- in the first element, in the middle or in
// the last -- and two arrays of different sizes are unequal (LRM 7.4, 7.5,
// 7.10, 11.4.5).
module Top;
  class Item;
    int value;
  endclass

  typedef struct {
    int a;
    logic [3:0] b;
  } pair_t;

  real reals [4] = '{1.5, 2.5, 3.5, 4.5};
  real same_reals [4] = '{1.5, 2.5, 3.5, 4.5};

  string words [$];
  string other_words [$];

  Item one, two;
  Item items [$];
  Item same_items [$];
  Item other_items [$];

  int rows [][2];
  int same_rows [][2];
  int other_rows [][2];

  pair_t pairs [3] = '{'{1, 4'h1}, '{2, 4'h2}, '{3, 4'h3}};
  pair_t unknown_pairs [3] = '{'{1, 4'h1}, '{2, 4'bx010}, '{3, 4'h3}};
  pair_t same_unknown_pairs [3] = '{'{1, 4'h1}, '{2, 4'bx010}, '{3, 4'h3}};

  int counted [];
  int first_differs [];
  int middle_differs [];
  int last_differs [];
  int shorter [];

  logic [3:0] fours [];
  logic [3:0] unknown_first [];
  logic [3:0] unknown_then_unequal [];

  logic reals_equal, reals_case_equal, reals_differ;
  logic words_equal, words_differ, words_case_equal;
  logic items_equal, items_differ, items_case_equal;
  logic rows_equal, rows_differ, rows_case_equal;
  logic pairs_equal, pairs_unknown, pairs_case_equal, pairs_case_differ;
  logic first_equal, middle_equal, last_equal, sizes_equal;
  logic first_case_equal, middle_case_equal, last_case_equal;
  logic sizes_case_equal;
  logic unknown_only, unknown_and_unequal;

  initial begin
    words = '{"alpha", "beta", "gamma"};
    other_words = '{"alpha", "beta", "delta"};

    one = new;
    two = new;
    items = '{one, two, null};
    same_items = '{one, two, null};
    other_items = '{one, one, null};

    rows = new[2];
    rows[0] = '{1, 2};
    rows[1] = '{3, 4};
    same_rows = new[2];
    same_rows[0] = '{1, 2};
    same_rows[1] = '{3, 4};
    other_rows = new[2];
    other_rows[0] = '{1, 2};
    other_rows[1] = '{3, 5};

    counted = '{1, 2, 3, 4, 5, 6, 7, 8};
    first_differs = '{9, 2, 3, 4, 5, 6, 7, 8};
    middle_differs = '{1, 2, 3, 9, 5, 6, 7, 8};
    last_differs = '{1, 2, 3, 4, 5, 6, 7, 9};
    shorter = '{1, 2, 3, 4, 5, 6, 7};

    fours = '{4'h1, 4'h2, 4'h3};
    unknown_first = '{4'bx001, 4'h2, 4'h3};
    unknown_then_unequal = '{4'bx001, 4'h2, 4'h9};

    reals_equal = 1'bz;
    reals_case_equal = 1'bz;
    reals_differ = 1'bz;
    words_equal = 1'bz;
    words_differ = 1'bz;
    words_case_equal = 1'bz;
    items_equal = 1'bz;
    items_differ = 1'bz;
    items_case_equal = 1'bz;
    rows_equal = 1'bz;
    rows_differ = 1'bz;
    rows_case_equal = 1'bz;
    pairs_equal = 1'bz;
    pairs_unknown = 1'bz;
    pairs_case_equal = 1'bz;
    pairs_case_differ = 1'bz;
    first_equal = 1'bz;
    middle_equal = 1'bz;
    last_equal = 1'bz;
    sizes_equal = 1'bz;
    first_case_equal = 1'bz;
    middle_case_equal = 1'bz;
    last_case_equal = 1'bz;
    sizes_case_equal = 1'bz;
    unknown_only = 1'bz;
    unknown_and_unequal = 1'bz;

    reals_equal = (reals == same_reals);
    reals_case_equal = (reals === same_reals);
    same_reals[3] = 0.5;
    reals_differ = (reals == same_reals);

    words_equal = (words == words);
    words_differ = (words == other_words);
    words_case_equal = (words === other_words);

    items_equal = (items == same_items);
    items_differ = (items == other_items);
    items_case_equal = (items === same_items);

    rows_equal = (rows == same_rows);
    rows_differ = (rows == other_rows);
    rows_case_equal = (rows === other_rows);

    pairs_equal = (pairs == pairs);
    pairs_unknown = (unknown_pairs == same_unknown_pairs);
    pairs_case_equal = (unknown_pairs === same_unknown_pairs);
    pairs_case_differ = (unknown_pairs === pairs);

    first_equal = (counted == first_differs);
    middle_equal = (counted == middle_differs);
    last_equal = (counted == last_differs);
    sizes_equal = (counted == shorter);
    first_case_equal = (counted === first_differs);
    middle_case_equal = (counted === middle_differs);
    last_case_equal = (counted === last_differs);
    sizes_case_equal = (counted === shorter);

    unknown_only = (fours == unknown_first);
    unknown_and_unequal = (fours == unknown_then_unequal);
  end

  final begin
    if (reals_equal !== 1'b1)
      $fatal(1, "reals_equal was %b, expected 1", reals_equal);
    if (reals_case_equal !== 1'b1)
      $fatal(1, "reals_case_equal was %b, expected 1", reals_case_equal);
    if (reals_differ !== 1'b0)
      $fatal(1, "reals_differ was %b, expected 0", reals_differ);

    if (words_equal !== 1'b1)
      $fatal(1, "words_equal was %b, expected 1", words_equal);
    if (words_differ !== 1'b0)
      $fatal(1, "words_differ was %b, expected 0", words_differ);
    if (words_case_equal !== 1'b0)
      $fatal(1, "words_case_equal was %b, expected 0", words_case_equal);

    if (items_equal !== 1'b1)
      $fatal(1, "items_equal was %b, expected 1", items_equal);
    if (items_differ !== 1'b0)
      $fatal(1, "items_differ was %b, expected 0", items_differ);
    if (items_case_equal !== 1'b1)
      $fatal(1, "items_case_equal was %b, expected 1", items_case_equal);

    if (rows_equal !== 1'b1)
      $fatal(1, "rows_equal was %b, expected 1", rows_equal);
    if (rows_differ !== 1'b0)
      $fatal(1, "rows_differ was %b, expected 0", rows_differ);
    if (rows_case_equal !== 1'b0)
      $fatal(1, "rows_case_equal was %b, expected 0", rows_case_equal);

    if (pairs_equal !== 1'b1)
      $fatal(1, "pairs_equal was %b, expected 1", pairs_equal);
    if (pairs_unknown !== 1'bx)
      $fatal(1, "pairs_unknown was %b, expected x", pairs_unknown);
    if (pairs_case_equal !== 1'b1)
      $fatal(1, "pairs_case_equal was %b, expected 1", pairs_case_equal);
    if (pairs_case_differ !== 1'b0)
      $fatal(1, "pairs_case_differ was %b, expected 0", pairs_case_differ);

    if (first_equal !== 1'b0)
      $fatal(1, "first_equal was %b, expected 0", first_equal);
    if (middle_equal !== 1'b0)
      $fatal(1, "middle_equal was %b, expected 0", middle_equal);
    if (last_equal !== 1'b0)
      $fatal(1, "last_equal was %b, expected 0", last_equal);
    if (sizes_equal !== 1'b0)
      $fatal(1, "sizes_equal was %b, expected 0", sizes_equal);
    if (first_case_equal !== 1'b0)
      $fatal(1, "first_case_equal was %b, expected 0", first_case_equal);
    if (middle_case_equal !== 1'b0)
      $fatal(1, "middle_case_equal was %b, expected 0", middle_case_equal);
    if (last_case_equal !== 1'b0)
      $fatal(1, "last_case_equal was %b, expected 0", last_case_equal);
    if (sizes_case_equal !== 1'b0)
      $fatal(1, "sizes_case_equal was %b, expected 0", sizes_case_equal);

    if (unknown_only !== 1'bx)
      $fatal(1, "unknown_only was %b, expected x", unknown_only);
    if (unknown_and_unequal !== 1'b0)
      $fatal(1, "unknown_and_unequal was %b, expected 0",
             unknown_and_unequal);
    $display("All checks passed");
  end
endmodule
