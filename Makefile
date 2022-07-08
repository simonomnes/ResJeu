SRC= tpjeux.ml
OBJ= $(SRC:.ml=.cmo)

all: tp_jeux

clean:
	rm *.cmi *.cmo

%.cmo: %.ml
	ocamlc -c $<

tp_jeux: $(OBJ)
	ocamlc $^ -o $@

