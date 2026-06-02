module Child;
  initial $display("child");
endmodule

module Test;
  Child c();
  initial $display("parent");
endmodule
