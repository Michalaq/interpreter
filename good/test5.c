{ // fajny sposob na podnoszenie do kwadratu / potegodwojkowe potegi dwojki 
    // test nr 5 //
    var compose :: (int => int) -> (int => int) => (int => int);
    var h :: (int => int);
    var i :: int;
    var j :: int;
    compose = fn (var f :: (int => int); var g :: (int => int)) => (int => int) {
                    return fn (var x :: int) => int { return f(g(x)); };
    };
    //h = fn (var x :: int) => int { return 2 * x; };
    i = 0;
    j = 1;
    while (i <= 5) do { // ostatni wynik to 2^(2^5)
        print h(j);
        h = compose(h, h);
        i++;
    };
};
