var p; {
  var x; {
    x = 1;
    p = proc y: print(x - y);
    var x; {
      x = 1 - 1 - 1;
      p(x);
    }
  }
}
