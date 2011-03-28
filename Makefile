# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of [kro]bot.

PREFIX := $(HOME)

OC := ocamlbuild -classic-display
OF := ocamlfind

.PHONY: all
all:
	$(OC) all

.PHONY: clean
clean:
	$(OC) -clean

.PHONY: install
install:
	$(OF) install krobot META \
	  $(wildcard lib-krobot/*.mli) \
	  $(wildcard common/*.mli) \
	  $(wildcard protocol/*.mli) \
	  $(wildcard _build/lib-krobot/*.cmi) \
	  $(wildcard _build/common/*.cmi) \
	  $(wildcard _build/protocol/*.cmi) \
	  $(wildcard _build/lib-krobot/*.cmx) \
	  $(wildcard _build/common/*.cmx) \
	  $(wildcard _build/protocol/*.cmx) \
	  $(wildcard _build/*.cma) \
	  $(wildcard _build/*.cmxa) \
	  $(wildcard _build/*.cmxs) \
	  $(wildcard _build/*.a)
	@/bin/bash install-programs.sh $(PREFIX)

.PHONY: uninstall
uninstall:
	$(OF) remove krobot
	rm -vf $(PREFIX)/bin/krobot-*
	rm -rvf $(PREFIX)/share/krobot

.PHONY: reinstall
reinstall: uninstall install
