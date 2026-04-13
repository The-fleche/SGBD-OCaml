# Makefile du projet de INPF12
# Pour compiler le projet taper simplement make en étant dans le répertoire du projet
# Pour nettoyer les fichiers générés taper make clean
# Ne pas modifier ce fichier à moins de comprendre ce que vous faites (ce qui n'est sans doute pas le cas)


FICHIER=projet.ml 
BIN=test

$(BIN): projet.cmo
	ocamlc -g projet.cmo -o $@

%.cmo : %.ml 
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


clean : 
	rm -f projet.cmo projet.cmi $(BIN)
