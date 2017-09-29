main:
	ocamlbuild -pkgs yojson,camlimages risk_map.byte deck.byte
	ocamlbuild -r -pkgs yojson risk_map.cmo
	ocamlbuild -pkgs yojson territory.cmo
	ocamlbuild -pkgs yojson player.cmo
	ocamlbuild -pkgs yojson card.cmo
	ocamlbuild -r -pkgs yojson deck.cmo
	ocamlbuild -r -pkgs yojson ai.cmo
	ocamlbuild  dice.cmo
	ocamlbuild -r -pkgs yojson,camlimages,camlimages.png,camlimages.graphics risk_Gui.cmo
	ocamlbuild game.ml
clean:
	ocamlbuild -clean
play:
	ocamlbuild -r -pkgs oUnit,yojson,str,ANSITerminal,camlimages,camlimages.png,camlimages.graphics,graphics main.byte && ./main.byte
