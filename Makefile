PREFIX = $(HOME)
BINDIR = $(PREFIX)/scripts
XMONADDIR = $(PREFIX)/.xmonad

install: install_scripts install_config

install_scripts:
	@echo Installing xmonad scripts to $(BINDIR)/
	@mkdir -p "$(BINDIR)"
	@cp -f scripts/*

install_config:
	@echo Installing xmonad config to $(XMONADDIR)/
	@mkdir -p "$(XMONADDIR)"
	@cp -f xmonad.hs "$(XMONADDIR)/"

.PHONY: install_config install_scripts install
