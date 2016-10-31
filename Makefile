all: grammar-cabal interpreter 

interpreter:
	happy -gca ParGramatyka.y
	alex -g LexGramatyka.x
	ghc --make interpreter.hs -o interpreter


grammar:
	-cp Makefile Makefile.bak
	bnfc -m -haskell Gramatyka.cf
	-rm -f Makefile
	mv Makefile.bak Makefile

grammar-cabal:
	-cp Makefile Makefile.bak
	cabal exec -- bnfc -m -haskell Gramatyka.cf
	-rm -f Makefile
	mv Makefile.bak Makefile

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGramatyka.* LexGramatyka.* ParGramatyka.* LayoutGramatyka.* SkelGramatyka.* PrintGramatyka.* TestGramatyka.* AbsGramatyka.* TestGramatyka interpreter ErrM.* SharedString.* ComposOp.* Gramatyka.dtd XMLGramatyka.*
