# Module: Host Makefile

# Help for make commands:

COMPILER := corebuild 
FILE := cli.native

help:
	@echo "Use: 'make <target>' where <target> is one of:"
	@echo "   run				Makes Native ocaml code and runs it"
	@echo "   native 			Makes Native ocaml code"

run: native
	./$(FILE)

native:
	$(COMPILER) -pkg yojson $(FILE)

.PHONY: native run help
