// Recognizes the text %p writes (LRM 21.2.1.6), for the corpus's cases whose
// subject that text is. The standard fixes what the text means and leaves its
// spelling open: white space anywhere outside a quoted string, an array's
// elements by position or under index labels, and a default element standing
// for those it covers. So a case states the value in full -- a structure's
// members by name, an array's elements by position, an associative array's
// entries under their indices -- and asks whether a printed text is one of the
// legal spellings of it. Only the syntax the cases use is read: atoms, quoted
// strings, nested patterns, keys and default.
//
// It compares structure and knows nothing of the value's type, so it is no
// validator of %p in general: it takes an array's index labels to count from
// 0, a key that is a number to make the value an associative array, an atom to
// be compared as written, and a default to stand for any element it is not
// told apart from. That holds for the forms the corpus's cases print -- arrays
// declared [N], structures with named members, associative arrays, enumeration
// names and decimal values -- and a case printing another form widens this
// first. The length limit a tool may set (1024 characters at least) is not
// modelled, since no case prints that much.
package assignment_pattern;

  // A printed value read as a tree: an atom, or a pattern of items, each under
  // a key or none.
  class Node;
    bit is_pattern;
    string atom;
    string keys[$];
    Node values[$];
  endclass

  // The reading and the comparison, as methods of a class: a recursive
  // function a package declares is one more thing a tool may not carry out,
  // and the recognizer leans on as little as it can.
  class Recognizer;

    static function bit IsSpace(byte c);
      return c inside {" ", "\t", "\n", 8'h0d};
    endfunction

    static function void SkipSpace(string text, ref int at);
      while (at < text.len() && IsSpace(text[at])) at++;
    endfunction

    // An atom runs to the next separator or space; a quoted string to its
    // closing quote, an escaped quote included.
    static function string ReadAtom(string text, ref int at);
      int start = at;
      if (text[at] == "\"") begin
        at++;
        while (at < text.len() && text[at] != "\"") begin
          if (text[at] == "\\") at++;
          at++;
        end
        at++;
        return text.substr(start, at - 1);
      end
      while (at < text.len() && !IsSpace(text[at]) &&
             !(text[at] inside {",", ":", "}"}))
        at++;
      return text.substr(start, at - 1);
    endfunction

    // The value at `at`, or null where the text there is no value.
    static function Node ReadValue(string text, ref int at);
      Node node = new;
      SkipSpace(text, at);
      if (at >= text.len()) return null;
      if (at + 1 < text.len() && text[at] == "'" && text[at+1] == "{") begin
        node.is_pattern = 1;
        at += 2;
        SkipSpace(text, at);
        if (at < text.len() && text[at] == "}") begin
          at++;
          return node;
        end
        forever begin
          Node first = ReadValue(text, at);
          string key = "";
          if (first == null) return null;
          SkipSpace(text, at);
          if (at < text.len() && text[at] == ":") begin
            if (first.is_pattern) return null;
            key = first.atom;
            at++;
            first = ReadValue(text, at);
            if (first == null) return null;
            SkipSpace(text, at);
          end
          node.keys.push_back(key);
          node.values.push_back(first);
          if (at >= text.len()) return null;
          if (text[at] == "}") begin
            at++;
            return node;
          end
          if (text[at] != ",") return null;
          at++;
        end
      end
      node.atom = ReadAtom(text, at);
      if (node.atom.len() == 0) return null;
      return node;
    endfunction

    static function Node Read(string text);
      int at = 0;
      Node node = ReadValue(text, at);
      SkipSpace(text, at);
      if (node == null || at != text.len()) return null;
      return node;
    endfunction

    static function bit IsIndex(string key);
      if (key.len() == 0) return 0;
      for (int i = 0; i < key.len(); i++)
        if (!(key[i] inside {["0":"9"]}) && !(i == 0 && key[i] == "-"))
          return 0;
      return 1;
    endfunction

    // The value `written` gives the item `expected` holds under `key` at
    // `position`: the one under that key, or at that position where none has
    // keys, else the default; null where it gives none.
    static function Node Covering(Node written, string key, int position,
                                  bit by_index);
      Node fallback = null;
      foreach (written.keys[i]) begin
        string k = written.keys[i];
        if (k == "default") fallback = written.values[i];
        else if (by_index && IsIndex(k) && k.atoi() == position)
          return written.values[i];
        else if (!by_index && k == key) return written.values[i];
      end
      return fallback;
    endfunction

    // Whether every key of `written` is one of `allowed` or default, the
    // default standing only where `default_allowed` says it may.
    static function bit KeysWithin(Node written, string allowed[$],
                                   bit default_allowed);
      foreach (written.keys[i]) begin
        bit found = 0;
        if (written.keys[i] == "default") begin
          if (!default_allowed) return 0;
          continue;
        end
        foreach (allowed[j]) if (allowed[j] == written.keys[i]) found = 1;
        if (!found) return 0;
      end
      return 1;
    endfunction

    static function bit Equivalent(Node written, Node expected);
      string names[$];
      bit positional = 1;
      bit indexed = expected.keys.size() > 0;
      if (!expected.is_pattern)
        return !written.is_pattern && written.atom == expected.atom;
      if (!written.is_pattern) return 0;
      foreach (expected.keys[i]) begin
        if (expected.keys[i] != "") positional = 0;
        if (!IsIndex(expected.keys[i])) indexed = 0;
      end

      // An array: its elements in order, or each under its index or covered by
      // a default.
      if (positional) begin
        string indices[$];
        bit by_position = 1;
        foreach (written.keys[i]) if (written.keys[i] != "") by_position = 0;
        if (by_position) begin
          if (written.values.size() != expected.values.size()) return 0;
          foreach (expected.values[i])
            if (!Equivalent(written.values[i], expected.values[i])) return 0;
          return 1;
        end
        foreach (expected.values[i]) indices.push_back($sformatf("%0d", i));
        if (!KeysWithin(written, indices, 1)) return 0;
        foreach (expected.values[i]) begin
          Node got = Covering(written, "", i, 1);
          if (got == null || !Equivalent(got, expected.values[i])) return 0;
        end
        return 1;
      end

      // An associative array: each entry under its own index, none left to a
      // default, since a default would add entries the array does not hold.
      if (indexed) begin
        if (written.keys.size() != expected.keys.size()) return 0;
        if (!KeysWithin(written, expected.keys, 0)) return 0;
        foreach (expected.keys[i]) begin
          Node got = Covering(written, expected.keys[i], 0, 0);
          if (got == null || !Equivalent(got, expected.values[i])) return 0;
        end
        return 1;
      end

      // A structure: each member under its name or covered by a default.
      names = expected.keys;
      if (!KeysWithin(written, names, 1)) return 0;
      foreach (expected.keys[i]) begin
        Node got = Covering(written, expected.keys[i], 0, 0);
        if (got == null || !Equivalent(got, expected.values[i])) return 0;
      end
      return 1;
    endfunction
  endclass

  // Whether `text` is a legal spelling of the value `expected` writes in full.
  function automatic bit Spells(string text, string expected);
    Node written = Recognizer::Read(text);
    Node wanted = Recognizer::Read(expected);
    if (wanted == null) $fatal(1, "the expected value '%s' does not parse", expected);
    return written != null && Recognizer::Equivalent(written, wanted);
  endfunction

endpackage
