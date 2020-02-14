all: compiler runtime clean

compiler:
	happy -gca src/ParLatte.y
	alex -g src/LexLatte.x
	ghc --make -XDeriveDataTypeable -isrc src/Main.hs -fno-cse -o latc_x86
runtime:
	mkdir lib
	gcc -m32 src/runtimec/runtime.c -o lib/runtime.o -c
clean:
	-rm -f src/*.log src/*.hi src/*.o src/*.dvi
	-rm -f DocLatte.ps
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.* Makefile*
