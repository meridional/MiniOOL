var p; {
  var q; {
    p = 1;
    q = 1;
    {
      Atom({
        q = p;
        q = q - 1 - 1 - 1 - 1;
        p = q;
      }) |||
      {
        print(p);
        p = p - 1;
        p = p - 1;
        print(p);
      }
    }
  }
};

var p; {
  var q; {
    p = 1;
    q = 1;
    {
      {
        q = p;
        q = q - 1 - 1 - 1 - 1;
        p = q;
      } |||
      Atom({
        print(p);
        p = p - 1;
        p = p - 1;
        print(p);
      })
    }
  }
}
