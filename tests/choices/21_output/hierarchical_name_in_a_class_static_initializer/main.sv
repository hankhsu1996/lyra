// %m prints the hierarchical name of the design element, subroutine, named
// block or labeled statement that invokes the task containing it (LRM
// 21.2.1.5). A class static property's initializer is written in a class,
// which is none of those, and the standard does not say what %m names there.
// Lyra names the class, after the package that declares it. This case holds
// Lyra to that.
package tools;
  class Tool;
    static string at_static = $sformatf("%m");
  endclass
endpackage

module Top;
  string in_static_property;

  function automatic bit starts_with(string text, string prefix);
    return text.len() >= prefix.len() &&
           text.substr(0, prefix.len() - 1) == prefix;
  endfunction

  function automatic bit ends_with(string text, string suffix);
    return text.len() >= suffix.len() &&
           text.substr(text.len() - suffix.len(), text.len() - 1) == suffix;
  endfunction

  initial in_static_property = tools::Tool::at_static;

  final begin
    if (!starts_with(in_static_property, "tools") ||
        !ends_with(in_static_property, "Tool"))
      $fatal(1, "%%m in a static property's initializer was %s",
             in_static_property);
    $display("All checks passed");
  end
endmodule
