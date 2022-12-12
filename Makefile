NEG_LUS:=$(wildcard tests/negatifs/*.lus)
NEG_FILE:= $(notdir $(NEG_LUS))
NEG_C:=$(NEG_FILE:.lus=.c)

exe:
	dune build
	mv ./src/minilucy.exe .

cleanall:
	dune clean
	rm ./minilucy.exe

.PHONY: tests_neg
tests_neg: $(NEG_C)

.PHONY: %.c
%.c:
	cd ./tests/negatifs/ && ../../minilucy.exe $(@:.c=.lus) -v main0 > $(@:.c=.lus).expected
	# cd ./tests/negatifs/ && ../../minilucy.exe $(@:.c=.lus) -v main0 > out || diff out $(@:.c=.lus).expected
