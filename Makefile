# Module: Host Makefile

# Help for make commands:

COMPILER := corebuild 
FILE := cli.native
INFERRED := cli.inferred.mli

help:
	@echo "Use: 'make <target>' where <target> is one of:"
	@echo "   run				Makes Native ocaml code and runs it"
	@echo "   native 			Makes Native ocaml code"
	@echo "   interface			Makes mli files"

run: native
	./$(FILE)

interface:
	$(COMPILER) -pkg yojson $(INFERRED)

native:
	$(COMPILER) -pkg yojson $(FILE)

.PHONY: interface native run help
