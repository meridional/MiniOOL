var p; {
  p = proc y: {
    print(y);
    if y < 1 - 1 - 1
      p = y
    else
      p(y - 1);
  };
  p(1);
}
