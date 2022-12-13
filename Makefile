NEG_LUS:=$(wildcard tests/negatifs/*.lus)
NEG_X:=$(NEG_LUS:.lus=.ex)

POS_LUS:=$(wildcard tests/positifs/*.lus)
POS_X:=$(POS_LUS:.lus=.ex)

exe:
	dune build
	mv ./src/minilucy.exe .

cleanall:
	dune clean
	rm ./minilucy.exe

%.ml:
	dune build
	mv ./src/minilucy.exe .

.PHONY: test
test: $(NEG_LUS) $(POS_LUS)
	rm out

.PHONY: %.lus
%.lus:
	@./minilucy.exe $@ main0 -v > out 2> /dev/null
	diff out $@.expected

.PHONY: promote
promote: $(NEG_X) $(POS_X)

.PHONY: %.ex
%.ex:
	./minilucy.exe $(@:.ex=.lus) main0 -v > $(@:.ex=.lus).expected 2> /dev/null
