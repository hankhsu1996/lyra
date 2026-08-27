// A foreach-loop over an associative array visits each of its allocated
// entries once, and the loop variable holds that entry's index; used anywhere
// other than as a subscript of the array it auto-casts to the array's index
// type. The index type is what imposes the order the entries are kept in, and
// so the order they are visited in: lexicographical from lesser to greater for
// a string index, and signed numerical for a signed integral index. An array
// with no entries allocated runs the body no times, and an associative
// dimension nests with a fixed one exactly as two dimensions of any other
// array do (LRM 12.7.3, LRM 7.8, LRM 7.8.2, LRM 7.8.4).
module Top;
  int by_string [string];
  string string_keys;
  int string_order;

  int by_int [int];
  int int_order;

  int empty_map [string];
  int empty_passes;

  int nested_map [string][int];
  int nested_order;
  int nested_passes;

  int map_of_fixed [string][0:1];
  int map_of_fixed_order;

  int fixed_of_map [2][string];
  int fixed_of_map_order;

  initial begin
    by_string["banana"] = 2;
    by_string["apple"] = 1;
    by_string["cherry"] = 3;
    string_keys = "";
    string_order = 0;
    foreach (by_string[k]) begin
      string_keys = {string_keys, k};
      string_order = string_order * 10 + by_string[k];
    end

    by_int[10] = 3;
    by_int[-5] = 2;
    by_int[5] = 1;
    int_order = 0;
    foreach (by_int[k]) int_order = int_order * 10 + by_int[k];

    empty_passes = 0;
    foreach (empty_map[k]) empty_passes = empty_passes + 1;

    nested_map["x"][2] = 2;
    nested_map["x"][1] = 1;
    nested_map["y"][1] = 3;
    nested_order = 0;
    nested_passes = 0;
    foreach (nested_map[i, j]) begin
      nested_order = nested_order * 10 + nested_map[i][j];
      nested_passes = nested_passes + 1;
    end

    map_of_fixed["p"][0] = 1;
    map_of_fixed["p"][1] = 2;
    map_of_fixed["q"][0] = 4;
    map_of_fixed["q"][1] = 3;
    map_of_fixed_order = 0;
    foreach (map_of_fixed[i, j])
      map_of_fixed_order = map_of_fixed_order * 10 + map_of_fixed[i][j];

    fixed_of_map[0]["a"] = 1;
    fixed_of_map[1]["a"] = 4;
    fixed_of_map[1]["b"] = 2;
    fixed_of_map_order = 0;
    foreach (fixed_of_map[i, k])
      fixed_of_map_order = fixed_of_map_order * 10 + fixed_of_map[i][k];
  end

  final begin
    if (string_keys !== "applebananacherry")
      $fatal(1, "string_keys was %s, expected applebananacherry",
             string_keys);
    if (string_order !== 123)
      $fatal(1, "string_order was %0d, expected 123", string_order);
    if (int_order !== 213)
      $fatal(1, "int_order was %0d, expected 213", int_order);
    if (empty_passes !== 0)
      $fatal(1, "empty_passes was %0d, expected 0", empty_passes);
    if (nested_order !== 123)
      $fatal(1, "nested_order was %0d, expected 123", nested_order);
    if (nested_passes !== 3)
      $fatal(1, "nested_passes was %0d, expected 3", nested_passes);
    if (map_of_fixed_order !== 1243)
      $fatal(1, "map_of_fixed_order was %0d, expected 1243",
             map_of_fixed_order);
    if (fixed_of_map_order !== 142)
      $fatal(1, "fixed_of_map_order was %0d, expected 142",
             fixed_of_map_order);
    $display("All checks passed");
  end
endmodule
