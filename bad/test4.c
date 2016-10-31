{ // nie podanie typow przy lambdzie 
    var test4 :: (int => int) -> (int => int) -> int => int;
     // zwykly syntax error, typy nalezy podac zawsze w definicji i deklaracji
    test4 = fn (var f; var g; var x) => int {
                    return f(g(x));
    };
    print test4(
          fn (var x) => int { return x + 5; },
          fn (var x) => int { return x * 2; },
          8);
    print test4(
          fn (var x) => int { return x * 2; },
          fn (var x) => int { return x + 5; },
          8);
};
