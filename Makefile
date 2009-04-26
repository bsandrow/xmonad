PREFIX      = $(HOME)
scripts_dir = $(PREFIX)/scripts
xmonad_dir  = $(PREFIX)/.xmonad

.PHONY: install install_scripts install_config

install: install_scripts install_config

install_config:
	@echo Installing xmonad config to $(xmonad_dir)/
	@mkdir -p "$(xmonad_dir)"
	@cp -f src/* "$(xmonad_dir)/"

install_scripts:
	@echo Installing scripts to $(script_dir)/
	@mkdir -p $(scripts_dir)/
	@cp -f scripts/* "$(scripts_dir)/"
