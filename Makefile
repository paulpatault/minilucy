exe:
	dune build
	mv ./src/minilucy.exe .

cleanall:
	dune clean
	rm ./minilucy.exe
