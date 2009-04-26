PREFIX = $(HOME)
BINDIR = $(PREFIX)/scripts
XMONADDIR = $(PREFIX)/.xmonad

install:
	@echo Installing xmonad config to $(XMONADDIR)/
	@mkdir -p "$(XMONADDIR)"
	@cp -f src/* "$(XMONADDIR)/"

.PHONY: install
