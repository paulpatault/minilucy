NEG_LUS:=$(wildcard tests/negatifs/*.lus)
NEG_FILE:= $(notdir $(NEG_LUS))
NEG_C:=$(NEG_FILE:.lus=.c)

NEG_C2:=$(NEG_FILE:.lus=.c2)

exe:
	dune build
	mv ./src/minilucy.exe .

cleanall:
	dune clean
	rm ./minilucy.exe

.PHONY: tests_neg
tests_neg: $(NEG_C)

.PHONY: promote
promote: $(NEG_C2)

.PHONY: %.c2
%.c2:
	./minilucy.exe ./tests/negatifs/$(@:.c2=.lus) main0 -v > ./tests/negatifs/$(@:.c2=.lus).expected 2> /dev/null

.PHONY: %.c
%.c:
	@./minilucy.exe ./tests/negatifs/$(@:.c=.lus) main0 -v > out 2> /dev/null
	diff out ./tests/negatifs/$(@:.c=.lus).expected
