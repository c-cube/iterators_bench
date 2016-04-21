
DEPS = sequence gen core_kernel benchmark

build: deps
	ocamlfind opt -O3 $(addprefix -package , $(DEPS)) -linkpkg bench.ml -o bench.native

deps:
	@echo install deps
	@opam install --yes gen sequence core_kernel benchmark

clean:
	rm *.{cm*,native}

run: build
	./bench.native
