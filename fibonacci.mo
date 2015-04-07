var add; {
  malloc(add);
  add.a = 1 - 1;
  add.b = 1 - 1;
  add.tmp = 1 - 1;
  add.f = proc y: var zero; {
    zero = 1 - 1;
    add.b = y;
    add.tmp = add.a;
    var minusOne; {
      minusOne = zero - 1;
      while zero < add.tmp {
        add.tmp = add.tmp - 1;
        add.b = add.b - minusOne; 
      }
    };
  };
  var mult; {
    malloc(mult);
    mult.a = 1;
    mult.b = 1 - 1;
    mult.tmp = 1 - 1;
    mult.f = proc y: {
      if y == 1 - 1 {
        mult.b = 1 - 1;
      }
      else {
        mult.f(y - 1);
        add.a = mult.b;
        add.f(mult.a);
        mult.b = add.b;
      }
    };
    var fib; {
      malloc(fib);
      fib.a = 1;
      fib.b = 1;
      fib.f = proc y : {
        add.a = fib.a;
        add.f(fib.b);
        fib.a = fib.b;
        fib.b = add.b;
      };
      var limit; {
        limit = 1;
        add.a = limit;
        add.f(limit);
        limit = add.b;

        mult.a = limit;
        mult.f(limit);
        limit = mult.b;

        mult.a = limit;
        mult.f(limit);
        limit = mult.b;

        var i; {
          i = 1;
          print(fib.a);
          while i < limit {
            fib.f(1);
            print(fib.a);
            add.a = i;
            add.f(1);
            i = add.b;
          }
        }
      }
    }
  }
}
