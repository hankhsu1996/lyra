// The array manipulation methods operate on any unpacked array, whatever its
// elements are (LRM 7.12). Where the relational operators are defined for the
// element type, as they are for string (LRM 6.16), a locator that compares and
// an ordering method take no with clause and compare the elements themselves;
// an unpacked structure has none, so the with clause supplies what is compared
// (LRM 7.12.1, 7.12.2). A locator returns a queue of the element type or of
// int, the first and the last element being those closest to the leftmost and
// the rightmost index, and the order a locator visits in is otherwise left
// open, so a result of several elements is checked for what it holds. An
// ordering method rearranges the elements of a fixed-size array where they
// are, strings and structures whole.
module Top;
  typedef struct {
    int rank;
    string name;
  } entry_t;

  string words[5] = '{"pear", "apple", "fig", "apple", "kiwi"};
  string words_down[5] = '{"pear", "apple", "fig", "apple", "kiwi"};
  string words_back[3] = '{"a", "b", "c"};
  string by_length[4] = '{"ccc", "a", "dddd", "bb"};
  entry_t entries[4] = '{'{3, "c"}, '{1, "a"}, '{4, "d"}, '{2, "b"}};
  entry_t entries_down[4] = '{'{3, "c"}, '{1, "a"}, '{4, "d"}, '{2, "b"}};
  entry_t entries_back[3] = '{'{1, "x"}, '{2, "y"}, '{3, "z"}};

  string found_words[$];
  int found_word_indices[$];
  string first_word[$];
  int last_word_index[$];
  string smallest_word[$];
  string largest_word[$];
  string distinct_words[$];
  entry_t found_entries[$];
  int found_entry_indices[$];
  entry_t first_entry[$];
  entry_t last_entry[$];
  entry_t top_entry[$];
  entry_t no_entries[$];

  initial begin
    found_words = words.find with (item > "g");
    found_word_indices = words.find_index with (item == "apple");
    first_word = words.find_first with (item.len() == 3);
    last_word_index = words.find_last_index with (item < "g");
    smallest_word = words.min;
    largest_word = words.max;
    distinct_words = words.unique;
    found_entries = entries.find with (item.rank > 2);
    found_entry_indices = entries.find_index with (item.name == "d");
    first_entry = entries.find_first with (item.rank < 3);
    last_entry = entries.find_last with (item.rank < 3);
    top_entry = entries.max with (item.rank);
    no_entries = '{'{9, "nine"}};
    no_entries = entries.find with (item.rank > 100);

    words.sort();
    words_down.rsort();
    words_back.reverse();
    by_length.sort with (item.len());
    entries.sort with (item.rank);
    entries_down.rsort(entry) with (entry.name);
    entries_back.reverse();
  end

  final begin
    if (found_words.size() !== 2 ||
        !((found_words[0] == "pear" && found_words[1] == "kiwi") ||
          (found_words[0] == "kiwi" && found_words[1] == "pear")))
      $fatal(1, "the strings after \"g\" were %p", found_words);
    if (found_word_indices.size() !== 2 ||
        found_word_indices[0] + found_word_indices[1] !== 4 ||
        found_word_indices[0] * found_word_indices[1] !== 3)
      $fatal(1, "the indices of \"apple\" were %p", found_word_indices);
    if (first_word.size() !== 1 || first_word[0] != "fig")
      $fatal(1, "the first string of three characters was %p", first_word);
    if (last_word_index.size() !== 1 || last_word_index[0] !== 3)
      $fatal(1, "the last index of a string before \"g\" was %p",
             last_word_index);
    if (smallest_word.size() !== 1 || smallest_word[0] != "apple")
      $fatal(1, "the smallest string was %p", smallest_word);
    if (largest_word.size() !== 1 || largest_word[0] != "pear")
      $fatal(1, "the largest string was %p", largest_word);
    if (distinct_words.size() !== 4)
      $fatal(1, "the distinct strings were %p", distinct_words);

    if (found_entries.size() !== 2 ||
        found_entries[0].rank + found_entries[1].rank !== 7 ||
        !((found_entries[0].name == "c" && found_entries[1].name == "d") ||
          (found_entries[0].name == "d" && found_entries[1].name == "c")))
      $fatal(1, "the structures ranked above 2 were %p", found_entries);
    if (found_entry_indices.size() !== 1 || found_entry_indices[0] !== 2)
      $fatal(1, "the index of the structure named \"d\" was %p",
             found_entry_indices);
    if (first_entry.size() !== 1 || first_entry[0].rank !== 1 ||
        first_entry[0].name != "a")
      $fatal(1, "the first structure ranked below 3 was %p", first_entry);
    if (last_entry.size() !== 1 || last_entry[0].rank !== 2 ||
        last_entry[0].name != "b")
      $fatal(1, "the last structure ranked below 3 was %p", last_entry);
    if (top_entry.size() !== 1 || top_entry[0].rank !== 4 ||
        top_entry[0].name != "d")
      $fatal(1, "the structure of the largest rank was %p", top_entry);
    if (no_entries.size() !== 0)
      $fatal(1, "a locator nothing satisfied returned %p", no_entries);

    if (words[0] != "apple" || words[1] != "apple" || words[2] != "fig" ||
        words[3] != "kiwi" || words[4] != "pear")
      $fatal(1, "strings sorted were %p", words);
    if (words_down[0] != "pear" || words_down[1] != "kiwi" ||
        words_down[2] != "fig" || words_down[3] != "apple" ||
        words_down[4] != "apple")
      $fatal(1, "strings sorted descending were %p", words_down);
    if (words_back[0] != "c" || words_back[1] != "b" || words_back[2] != "a")
      $fatal(1, "strings reversed were %p", words_back);
    if (by_length[0] != "a" || by_length[1] != "bb" || by_length[2] != "ccc" ||
        by_length[3] != "dddd")
      $fatal(1, "strings sorted by length were %p", by_length);
    if (entries[0].rank !== 1 || entries[0].name != "a" ||
        entries[1].rank !== 2 || entries[1].name != "b" ||
        entries[2].rank !== 3 || entries[2].name != "c" ||
        entries[3].rank !== 4 || entries[3].name != "d")
      $fatal(1, "structures sorted by rank were %p", entries);
    if (entries_down[0].name != "d" || entries_down[0].rank !== 4 ||
        entries_down[1].name != "c" || entries_down[1].rank !== 3 ||
        entries_down[2].name != "b" || entries_down[2].rank !== 2 ||
        entries_down[3].name != "a" || entries_down[3].rank !== 1)
      $fatal(1, "structures sorted descending by name were %p", entries_down);
    if (entries_back[0].rank !== 3 || entries_back[0].name != "z" ||
        entries_back[1].rank !== 2 || entries_back[1].name != "y" ||
        entries_back[2].rank !== 1 || entries_back[2].name != "x")
      $fatal(1, "structures reversed were %p", entries_back);
    $display("All checks passed");
  end
endmodule
