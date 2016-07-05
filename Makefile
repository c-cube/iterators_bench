
DEPS = sequence gen core_kernel benchmark containers

OPTS = -O3 -unbox-closures -unbox-closures-factor 20

all: deps build

build:
	ocamlfind opt $(OPTS) $(addprefix -package , $(DEPS)) -linkpkg bench.ml -o bench.native

deps:
	@echo install deps
	@opam install --yes $(DEPS)

clean:
	rm *.{cm*,native}

run: build
	./bench.native
