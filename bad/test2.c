var test2 :: => int;

// nieprawidlowe typy zwracane przez funkcje
test2 = fn() => int {
    var f :: int -> int => int;
    var g :: int -> int => int;
    var i :: int;
    var j :: int;
    
    f = fn (var x :: int; var y :: int ) => int { return x > y; }; // typ returna sie nie zgadza
    g = fn (var x :: int; var y :: int ) => int { return x * 2 > y; }; // tu rowniez
    
    i = 0;
    i += 5;
    j = 9;
    
    {
        if f(i, j) then {
            print 1;
        } else {
            if g(i, j) then {
                print 2;
            } else {
                print 5;
            } end;
        } end;
        {
            var i :: int;
            i = j;
            if not f(i, j) then {
                print 3;
            } else {
                print 4;
            } end;
        };
    };
    return 1;
};

{
    var x :: int;
    x = test2();
};
