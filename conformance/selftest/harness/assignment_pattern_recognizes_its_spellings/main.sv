// @includes: assignment_pattern.sv
//
// The corpus's recognizer of %p text, held to what LRM 21.2.1.6 allows a tool
// to print for a value and what it does not, on texts written here by hand.
// A case relying on the recognizer is only as good as this, so every tool runs
// it: one that fails it shows the recognizer, not the tool, to be wrong.
module Top;
  import assignment_pattern::Spells;

  task automatic legal(string text, string value);
    if (!Spells(text, value))
      $fatal(1, "'%s' was not taken as a spelling of %s", text, value);
  endtask

  task automatic illegal(string text, string value);
    if (Spells(text, value))
      $fatal(1, "'%s' was taken as a spelling of %s", text, value);
  endtask

  initial begin
    // A singular value, with white space around it or none, but never within
    // it: one token is one value.
    legal("ON", "ON");
    legal("         7", "7");
    legal("\"a b\"", "\"a b\"");
    illegal("OFF", "ON");
    illegal("\"ab\"", "\"a b\"");
    illegal("1 0", "10");
    illegal("O N", "ON");

    // A comma, a colon or a brace inside a quoted string is part of it.
    legal("\"a,b:c}\"", "\"a,b:c}\"");
    legal("'{s:\"x, y\", n:1}", "'{s:\"x, y\", n:1}");
    illegal("'{s:\"x\", y\", n:1}", "'{s:\"x, y\", n:1}");

    // A structure: its members by name, in any spacing and any order, some of
    // them under a default.
    legal("'{sw:OFF, count:3}", "'{sw:OFF, count:3}");
    legal("'{ sw : OFF ,count: 3 }", "'{sw:OFF, count:3}");
    legal("'{count:3, sw:OFF}", "'{sw:OFF, count:3}");
    legal("'{sw:OFF, default:3}", "'{sw:OFF, count:3}");
    legal("'{default:7}", "'{a:7, b:7}");
    illegal("'{sw:ON, count:3}", "'{sw:OFF, count:3}");
    illegal("'{sw:OFF}", "'{sw:OFF, count:3}");
    illegal("'{sw:OFF, count:3, extra:1}", "'{sw:OFF, count:3}");
    illegal("'{OFF, 3}", "'{sw:OFF, count:3}");
    illegal("'{sw:OFF, default:4}", "'{sw:OFF, count:3}");

    // An array: by position, under index labels, or under a default.
    legal("'{ON, OFF}", "'{ON, OFF}");
    legal("'{0:ON, 1:OFF}", "'{ON, OFF}");
    legal("'{1:OFF, default:ON}", "'{ON, OFF}");
    illegal("'{OFF, ON}", "'{ON, OFF}");
    illegal("'{ON}", "'{ON, OFF}");
    illegal("'{0:ON, 2:OFF}", "'{ON, OFF}");

    // An associative array: every entry under its own index.
    legal("'{5:100, 300:7}", "'{5:100, 300:7}");
    legal("'{300:7,5:100}", "'{5:100, 300:7}");
    illegal("'{5:100}", "'{5:100, 300:7}");
    illegal("'{5:100, default:7}", "'{5:100, 300:7}");

    // An index label naming no element is no spelling of the array.
    illegal("'{0:ON, 1:OFF, 2:ON}", "'{ON, OFF}");
    illegal("'{-1:ON, 1:OFF}", "'{ON, OFF}");

    // A pattern within a pattern, a default inside one included, and text
    // that is no pattern: an apostrophe parted from its brace starts none.
    legal("'{holds:'{1:10}, weight:2}", "'{holds:'{1:10}, weight:2}");
    legal("'{row:'{default:4}, n:1}", "'{row:'{4, 4}, n:1}");
    illegal("'{row:'{default:5}, n:1}", "'{row:'{4, 4}, n:1}");
    illegal("'{holds:'{1:11}, weight:2}", "'{holds:'{1:10}, weight:2}");
    illegal("'{sw:OFF, count:3", "'{sw:OFF, count:3}");
    illegal("' {sw:OFF, count:3}", "'{sw:OFF, count:3}");
    illegal("'(sw:OFF, count:3}", "'{sw:OFF, count:3}");
    illegal("", "ON");

    $display("All checks passed");
  end
endmodule
