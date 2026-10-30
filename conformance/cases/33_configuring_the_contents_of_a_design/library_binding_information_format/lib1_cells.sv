module Same;
  string binding = "unset";
  initial binding = $sformatf("%l");
endmodule

module Sized #(parameter int Width = 1);
  logic [Width-1:0] data;
  string binding = "unset";
  initial binding = $sformatf("%l");
endmodule
