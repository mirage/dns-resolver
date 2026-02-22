vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

pagejaune.hvt.target: | vendors
	@echo " BUILD pagejaune.exe"
	@dune build --root . --profile=release ./pagejaune.exe
	@echo " DESCR pagejaune.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./pagejaune.exe 1> $@ 2>&1)

pagejaune.hvt: pagejaune.hvt.target
	@echo " COPY pagejaune.hvt"
	@cp $(file < pagejaune.hvt.target) $@
	@chmod +w $@
	@echo " STRIP pagejaune.hvt"
	@strip $@

pagejaune.install: pagejaune.hvt
	@echo " GEN pagejaune.install"
	@ocaml install.ml > $@

all: pagejaune.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f pagejaune.hvt.target
	rm -f pagejaune.hvt
	rm -f pagejaune.install

install: pagejaune.intall
	@echo " INSTALL pagejaune"
	opam-installer pagejaune.install
