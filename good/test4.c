{ // zlozenie funkcji anonimowych na rozne sposoby 
    // test nr 4 //
    var compose :: (int => int) -> (int => int) => (int => int);
    var f :: (int => int);
    var g :: (int => int);
    var h :: (int => int);
    compose = fn (var f :: (int => int); var g :: (int => int)) => (int => int) {
                    return fn (var x :: int) => int { return f(g(x)); };
    };
    f = compose(
          fn (var x :: int) => int { return x + 5; },
          fn (var x :: int) => int { return x * 2; }
          );
    g = compose(
          fn (var x :: int) => int { return x * 2; },
          fn (var x :: int) => int { return x + 5; }
          );
    print f(8);
    print g(8);
    h = compose(f, g);
    print h(8);
    h = compose(g, f);
    print h(8);
};
